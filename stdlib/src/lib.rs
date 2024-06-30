use std::ffi::c_char;

mod str;

#[no_mangle]
extern "C" fn myl_print(string: *const c_char) {
    let string = unsafe { std::ffi::CStr::from_ptr(string) };
    println!("{}", string.to_str().unwrap());
}

#[no_mangle]
extern "C" fn myl_sin(x: f64) -> f64 {
    x.sin()
}

#[no_mangle]
extern "C" fn myl_cos(x: f64) -> f64 {
    x.cos()
}

#[no_mangle]
extern "C" fn myl_tan(x: f64) -> f64 {
    x.tan()
}

#[no_mangle]
extern "C" fn myl_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[no_mangle]
extern "C" fn myl_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}
