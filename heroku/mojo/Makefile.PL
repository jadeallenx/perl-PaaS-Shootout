use ExtUtils::MakeMaker;

WriteMakefile( 
    PREREQ_PM => {
        "Mojolicious" => 0,
        "Try::Tiny" => 0,
        "Regexp::Common" => 0,
        "DateTime" => 0,
        "DateTime::TimeZone" => 0,
    },
);

