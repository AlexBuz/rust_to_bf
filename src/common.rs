macro_rules! debug_print {
    ($($arg:tt)*) => {
        if $crate::DEBUG.load($crate::Ordering::Relaxed) {
            print!($($arg)*);
        }
    };
}

macro_rules! debug_println {
    ($($arg:tt)*) => {
        if $crate::DEBUG.load($crate::Ordering::Relaxed) {
            println!($($arg)*);
        }
    };
}

macro_rules! indented_print {
    ($depth:expr, $($arg:tt)*) => {
        $crate::common::debug_print!("{:width$}", "", width = $depth * 4);
        $crate::common::debug_print!($($arg)*);
    };
}

macro_rules! indented_println {
    ($depth:expr, $($arg:tt)*) => {
        $crate::common::debug_print!("{:width$}", "", width = $depth * 4);
        $crate::common::debug_println!($($arg)*);
    };
    () => {
        $crate::common::debug_println!();
    };
}

pub(crate) use debug_print;
pub(crate) use debug_println;
pub(crate) use indented_print;
pub(crate) use indented_println;
