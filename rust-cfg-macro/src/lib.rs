#![no_std]

#[macro_export]
macro_rules! feature {
    (if $name:literal { $then:expr } else { $else:expr }) => {
        (|| {
                                                                    #[cfg(feature = $name)]
                                                                    return $then;
                                                                    #[cfg(not(feature = $name))]
                                                                    return $else;
                                                                }())
    };
}
