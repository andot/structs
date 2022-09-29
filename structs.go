// Package structs contains various utilities functions to work with structs.
package structs

import (
	"fmt"
	"sync"
	"unsafe"

	"reflect"

	"github.com/modern-go/reflect2"
)

var (
	// DefaultTagName is the default tag name for struct fields which provides
	// a more granular to tweak certain structs. Lookup the necessary functions
	// for more info.
	DefaultTagName = "structs" // struct's field default tag name

	structInfoCache     = map[reflect.Type]*structInfo{}
	structInfoCacheLock sync.RWMutex
)

type fieldAccessor struct {
	t          reflect2.Type
	alias      string
	field      *reflect2.UnsafeStructField
	zero       interface{}
	omitempty  bool
	omitnested bool
	tostring   bool
	flatten    bool
	isStruct   bool
	unexported bool
}

type structInfo struct {
	t        *reflect2.UnsafeStructType
	name     string
	names    []string
	fields   []*fieldAccessor
	fieldMap map[string]*fieldAccessor
}

func newStructInfo(t reflect.Type, tagName string) (info *structInfo) {
	t2 := reflect2.Type2(t).(*reflect2.UnsafeStructType)
	fieldAccessors := getfieldAccessors(t2, tagName)
	n := len(fieldAccessors)
	info = &structInfo{
		t:        t2,
		name:     t2.Name(),
		names:    make([]string, 0, n),
		fields:   fieldAccessors,
		fieldMap: make(map[string]*fieldAccessor, n),
	}
	for _, f := range fieldAccessors {
		name := f.field.Name()
		if !f.unexported {
			info.names = append(info.names, name)
		}
		info.fieldMap[name] = f
	}
	return info
}

func getfieldAccessors(t reflect2.StructType, tagName string) (fields []*fieldAccessor) {
	for i, n := 0, t.NumField(); i < n; i++ {
		f := t.Field(i)
		ft := f.Type()
		tag := f.Tag().Get(tagName)
		if tag == "-" {
			continue
		}
		alias, opitons := parseTag(tag)
		if alias == "" {
			alias = f.Name()
		}
		typ := ft.Type1()
		field := &fieldAccessor{
			t:          ft,
			alias:      alias,
			field:      f.(*reflect2.UnsafeStructField),
			zero:       reflect.Zero(typ).Interface(),
			omitempty:  opitons.Has("omitempty"),
			omitnested: opitons.Has("omitnested"),
			tostring:   opitons.Has("string"),
			flatten:    opitons.Has("flatten"),
			unexported: f.PkgPath() != "",
		}
		if ft.Kind() == reflect.Ptr {
			ft = ft.(*reflect2.UnsafePtrType).Elem()
		}
		field.isStruct = (ft.Kind() == reflect.Struct)
		field.flatten = field.flatten && !field.omitnested && (ft.Kind() == reflect.Map || field.isStruct)
		fields = append(fields, field)
	}
	return fields
}

func getStructInfo(t reflect.Type, tagName string) *structInfo {
	structInfoCacheLock.RLock()
	if info, ok := structInfoCache[t]; ok {
		structInfoCacheLock.RUnlock()
		return info
	}
	structInfoCacheLock.RUnlock()
	structInfoCacheLock.Lock()
	info := newStructInfo(t, tagName)
	structInfoCache[t] = info
	structInfoCacheLock.Unlock()
	return info
}

// Struct encapsulates a struct type to provide several high level functions
// around the struct.
type Struct struct {
	raw     interface{}
	value   reflect.Value
	tagName string
	*structInfo
}

// New returns a new *Struct with the struct s. It panics if the s's kind is
// not struct.
func New(s interface{}, tagName ...string) *Struct {
	name := DefaultTagName
	if len(tagName) > 0 {
		name = tagName[0]
	}
	value := strctVal(s)
	return &Struct{
		raw:        s,
		value:      value,
		tagName:    name,
		structInfo: getStructInfo(value.Type(), name),
	}
}

// Map converts the given struct to a map[string]interface{}, where the keys
// of the map are the field names and the values of the map the associated
// values of the fields. The default key string is the struct field name but
// can be changed in the struct field's tag value. The "structs" key in the
// struct's field tag value is the key name. Example:
//
//	// Field appears in map as key "myName".
//	Name string `structs:"myName"`
//
// A tag value with the content of "-" ignores that particular field. Example:
//
//	// Field is ignored by this package.
//	Field bool `structs:"-"`
//
// A tag value with the content of "string" uses the stringer to get the value. Example:
//
//	// The value will be output of Animal's String() func.
//	// Map will panic if Animal does not implement String().
//	Field *Animal `structs:"field,string"`
//
// A tag value with the option of "flatten" used in a struct field is to flatten its fields
// in the output map. Example:
//
//	// The FieldStruct's fields will be flattened into the output map.
//	FieldStruct time.Time `structs:",flatten"`
//
// A tag value with the option of "omitnested" stops iterating further if the type
// is a struct. Example:
//
//	// Field is not processed further by this package.
//	Field time.Time     `structs:"myName,omitnested"`
//	Field *http.Request `structs:",omitnested"`
//
// A tag value with the option of "omitempty" ignores that particular field if
// the field value is empty. Example:
//
//	// Field appears in map as key "myName", but the field is
//	// skipped if empty.
//	Field string `structs:"myName,omitempty"`
//
//	// Field appears in map as key "Field" (the default), but
//	// the field is skipped if empty.
//	Field string `structs:",omitempty"`
//
// Note that only exported fields of a struct can be accessed, non exported
// fields will be neglected.
func (s *Struct) Map() map[string]interface{} {
	return s.MapWithPrefix("")
}

func (s *Struct) MapWithPrefix(prefix string) map[string]interface{} {
	out := make(map[string]interface{})
	s.FillMapWithPrefix(out, prefix)
	return out
}

// FillMap is the same as Map. Instead of returning the output, it fills the
// given map.
func (s *Struct) FillMap(out map[string]interface{}) {
	s.FillMapWithPrefix(out, "")
}

func (s *Struct) FillMapWithPrefix(out map[string]interface{}, prefix string) {
	if out == nil {
		return
	}
	p := reflect2.PtrOf(s.raw)
	for _, f := range s.fields {
		// we can't access the value of unexported fields
		if f.unexported {
			continue
		}
		name := f.alias
		val := f.t.UnsafeIndirect(f.field.UnsafeGet(p))
		if f.omitempty {
			if reflect.DeepEqual(f.zero, val) {
				continue
			}
		}
		if f.tostring {
			if s, ok := val.(fmt.Stringer); ok {
				out[prefix+name] = s.String()
			}
			continue
		}
		if !f.omitnested {
			val = s.nested(val)
		}
		if f.flatten {
			if m, ok := val.(map[string]interface{}); ok {
				for k, v := range m {
					out[prefix+k] = v
				}
			}
		} else {
			out[prefix+name] = val
		}
	}
}

// Values converts the given s struct's field values to a []interface{}.  A
// struct tag with the content of "-" ignores the that particular field.
// Example:
//
//	// Field is ignored by this package.
//	Field int `structs:"-"`
//
// A value with the option of "omitnested" stops iterating further if the type
// is a struct. Example:
//
//	// Fields is not processed further by this package.
//	Field time.Time     `structs:",omitnested"`
//	Field *http.Request `structs:",omitnested"`
//
// A tag value with the option of "omitempty" ignores that particular field and
// is not added to the values if the field value is empty. Example:
//
//	// Field is skipped if empty
//	Field string `structs:",omitempty"`
//
// Note that only exported fields of a struct can be accessed, non exported
// fields  will be neglected.
func (s *Struct) Values() (values []interface{}) {
	p := reflect2.PtrOf(s.raw)
	for _, f := range s.fields {
		// we can't access the value of unexported fields
		if f.unexported {
			continue
		}
		val := f.t.UnsafeIndirect(f.field.UnsafeGet(p))
		if f.omitempty {
			if reflect.DeepEqual(f.zero, val) {
				continue
			}
		}
		if f.tostring {
			if s, ok := val.(fmt.Stringer); ok {
				values = append(values, s.String())
			}
		}
		if f.isStruct && !f.omitnested {
			values = append(values, New(val, s.tagName).Values()...)
		} else {
			values = append(values, val)
		}
	}
	return
}

// Fields returns a slice of Fields. A struct tag with the content of "-"
// ignores the checking of that particular field. Example:
//
//	// Field is ignored by this package.
//	Field bool `structs:"-"`
//
// It panics if s's kind is not struct.
func (s *Struct) Fields() (fields []*Field) {
	for _, f := range s.fields {
		fields = append(fields, &Field{
			value:      s.value.FieldByIndex(f.field.Index()),
			field:      f.field.StructField,
			defaultTag: s.tagName,
		})
	}
	return fields
}

// Names returns a slice of field names. A struct tag with the content of "-"
// ignores the checking of that particular field. Example:
//
//	// Field is ignored by this package.
//	Field bool `structs:"-"`
//
// It panics if s's kind is not struct.
func (s *Struct) Names() []string {
	names := make([]string, len(s.names))
	copy(names, s.names)
	return names
}

// Field returns a new Field struct that provides several high level functions
// around a single struct field entity. It panics if the field is not found.
func (s *Struct) Field(name string) *Field {
	f, ok := s.FieldOk(name)
	if !ok {
		panic("field not found")
	}
	return f
}

// FieldOk returns a new Field struct that provides several high level functions
// around a single struct field entity. The boolean returns true if the field
// was found.
func (s *Struct) FieldOk(name string) (*Field, bool) {
	f, ok := s.fieldMap[name]
	if !ok {
		return nil, false
	}
	return &Field{
		value:      s.value.FieldByIndex(f.field.Index()),
		field:      f.field.StructField,
		defaultTag: s.tagName,
	}, true
}

// IsZero returns true if all fields in a struct is a zero value (not
// initialized) A struct tag with the content of "-" ignores the checking of
// that particular field. Example:
//
//	// Field is ignored by this package.
//	Field bool `structs:"-"`
//
// A value with the option of "omitnested" stops iterating further if the type
// is a struct. Example:
//
//	// Field is not processed further by this package.
//	Field time.Time     `structs:"myName,omitnested"`
//	Field *http.Request `structs:",omitnested"`
//
// Note that only exported fields of a struct can be accessed, non exported
// fields  will be neglected. It panics if s's kind is not struct.
func (s *Struct) IsZero() bool {
	p := reflect2.PtrOf(s.raw)
	for _, f := range s.fields {
		// we can't access the value of unexported fields
		if f.unexported {
			continue
		}
		val := f.t.UnsafeIndirect(f.field.UnsafeGet(p))
		if f.isStruct && !f.omitnested {
			if !New(val, s.tagName).IsZero() {
				return false
			}
			continue
		}
		if !reflect.DeepEqual(f.zero, val) {
			return false
		}
	}
	return true
}

// HasZero returns true if a field in a struct is not initialized (zero value).
// A struct tag with the content of "-" ignores the checking of that particular
// field. Example:
//
//	// Field is ignored by this package.
//	Field bool `structs:"-"`
//
// A value with the option of "omitnested" stops iterating further if the type
// is a struct. Example:
//
//	// Field is not processed further by this package.
//	Field time.Time     `structs:"myName,omitnested"`
//	Field *http.Request `structs:",omitnested"`
//
// Note that only exported fields of a struct can be accessed, non exported
// fields  will be neglected. It panics if s's kind is not struct.
func (s *Struct) HasZero() bool {
	p := reflect2.PtrOf(s.raw)
	for _, f := range s.fields {
		// we can't access the value of unexported fields
		if f.unexported {
			continue
		}
		val := f.t.UnsafeIndirect(f.field.UnsafeGet(p))
		if f.isStruct && !f.omitnested {
			if New(val, s.tagName).HasZero() {
				return true
			}
			continue
		}
		if reflect.DeepEqual(f.zero, val) {
			return true
		}
	}
	return false
}

// Name returns the structs's type name within its package. For more info refer
// to Name() function.
func (s *Struct) Name() string {
	return s.t.Name()
}

func strctVal(s interface{}) reflect.Value {
	v := reflect.ValueOf(s)

	// if pointer get the underlying element≤
	for v.Kind() == reflect.Ptr {
		v = v.Elem()
	}

	if v.Kind() != reflect.Struct {
		panic("not struct")
	}

	return v
}

// Map converts the given struct to a map[string]interface{}. For more info
// refer to Struct types Map() method. It panics if s's kind is not struct.
func Map(s interface{}) map[string]interface{} {
	return New(s).Map()
}

func MapWithPrefix(s interface{}, prefix string) map[string]interface{} {
	return New(s).MapWithPrefix(prefix)
}

// FillMap is the same as Map. Instead of returning the output, it fills the
// given map.
func FillMap(s interface{}, out map[string]interface{}) {
	New(s).FillMap(out)
}

func FillMapWithPrefix(s interface{}, out map[string]interface{}, prefix string) {
	New(s).FillMapWithPrefix(out, prefix)
}

// Values converts the given struct to a []interface{}. For more info refer to
// Struct types Values() method.  It panics if s's kind is not struct.
func Values(s interface{}) []interface{} {
	return New(s).Values()
}

// Fields returns a slice of *Field. For more info refer to Struct types
// Fields() method.  It panics if s's kind is not struct.
func Fields(s interface{}) []*Field {
	return New(s).Fields()
}

// Names returns a slice of field names. For more info refer to Struct types
// Names() method.  It panics if s's kind is not struct.
func Names(s interface{}) []string {
	return New(s).Names()
}

// IsZero returns true if all fields is equal to a zero value. For more info
// refer to Struct types IsZero() method.  It panics if s's kind is not struct.
func IsZero(s interface{}) bool {
	return New(s).IsZero()
}

// HasZero returns true if any field is equal to a zero value. For more info
// refer to Struct types HasZero() method.  It panics if s's kind is not struct.
func HasZero(s interface{}) bool {
	return New(s).HasZero()
}

// IsStruct returns true if the given variable is a struct or a pointer to
// struct.
func IsStruct(s interface{}) bool {
	v := reflect.ValueOf(s)
	if v.Kind() == reflect.Ptr {
		v = v.Elem()
	}

	return v.Kind() == reflect.Struct
}

// Name returns the structs's type name within its package. It returns an
// empty string for unnamed types. It panics if s's kind is not struct.
func Name(s interface{}) string {
	return New(s).Name()
}

// nested retrieves recursively all types for the given value and returns the
// nested value.
func (s *Struct) nested(val interface{}) interface{} {
	v := reflect.ValueOf(val)
	if v.Kind() == reflect.Ptr {
		v = v.Elem()
	}
	switch v.Kind() {
	case reflect.Struct:
		m := New(val, s.tagName).Map()
		if len(m) == 0 {
			return val
		}
		return m
	case reflect.Map:
		t := v.Type()
		et := t.Elem()
		if et.Kind() == reflect.Ptr {
			et = et.Elem()
		}
		if t.Key().Kind() == reflect.String && (et.Kind() == reflect.Struct ||
			((et.Kind() == reflect.Slice || et.Kind() == reflect.Array) && et.Elem().Kind() == reflect.Struct)) {
			m := make(map[string]interface{}, v.Len())
			p := reflect2.PtrOf(val)
			mt := reflect2.Type2(t).(*reflect2.UnsafeMapType)
			iter := mt.UnsafeIterate(unsafe.Pointer(&p))
			kt := mt.Key()
			vt := mt.Elem()
			for iter.HasNext() {
				kp, vp := iter.UnsafeNext()
				m[kt.UnsafeIndirect(kp).(string)] = s.nested(vt.UnsafeIndirect(vp))
			}
			return m
		}
		return val
	case reflect.Slice, reflect.Array:
		et := v.Type().Elem()
		if et.Kind() != reflect.Struct &&
			!(et.Kind() == reflect.Ptr && et.Elem().Kind() == reflect.Struct) {
			return val
		}
		val = v.Interface()
		if v.Kind() == reflect.Array {
			val = toSlice(val)
		}
		n := (*reflect.SliceHeader)(reflect2.PtrOf(val)).Len
		slices := make([]interface{}, n)
		t := reflect2.TypeOf(val).(*reflect2.UnsafeSliceType)
		et2 := t.Elem()
		ptr := reflect2.PtrOf(val)
		for i := 0; i < n; i++ {
			slices[i] = s.nested(et2.UnsafeIndirect(t.UnsafeGetIndex(ptr, i)))
		}
		return slices
	}
	return val
}

type eface struct {
	typ unsafe.Pointer
	ptr unsafe.Pointer
}

func unpackEFace(ptr *interface{}) *eface {
	return (*eface)(unsafe.Pointer(ptr))
}

// sliceHeader is a safe version of SliceHeader used within this package.
type sliceHeader struct {
	Data unsafe.Pointer
	Len  int
	Cap  int
}

func unsafeToSlice(array interface{}, count int) unsafe.Pointer {
	return unsafe.Pointer(&sliceHeader{
		Data: reflect2.PtrOf(array),
		Len:  count,
		Cap:  count,
	})
}

func toSlice(array interface{}) (slice interface{}) {
	t := reflect.TypeOf(array)
	sliceType := reflect.SliceOf(t.Elem())
	sliceStruct := unpackEFace(&slice)
	sliceStruct.typ = reflect2.PtrOf(sliceType)
	sliceStruct.ptr = unsafeToSlice(array, t.Len())
	return
}
