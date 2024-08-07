macro_rules! debug_print {
    ($($arg:tt)*) => {
        if $crate::config::DEBUG {
            print!($($arg)*);
        }
    };
}

macro_rules! debug_println {
    ($($arg:tt)*) => {
        if $crate::config::DEBUG {
            println!($($arg)*);
        }
    };
}

macro_rules! indented_println {
    ($depth:expr, $($arg:tt)*) => {
        $crate::macros::debug_print!("{:width$}", "", width = $depth * 4);
        $crate::macros::debug_println!($($arg)*);
    };
    () => {
        $crate::macros::debug_println!();
    };
}

pub(crate) use debug_print;
pub(crate) use debug_println;
pub(crate) use indented_println;
