use std::ffi::{c_char, CStr, CString};

#[no_mangle]
unsafe extern "C" fn int_string(x: i32, buf: *mut c_char) {
    let string = CString::new("%d").unwrap();

    ::libc::sprintf(buf, string.as_ptr(), x);
}

#[no_mangle]
unsafe extern "C" fn bool_string(x: i32, buf: *mut c_char) {
    let string = CString::new("%d").unwrap();

    ::libc::sprintf(buf, string.as_ptr(), x);
}
