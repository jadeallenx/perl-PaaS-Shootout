use Dancer;

use Try::Tiny;
use Regexp::Common qw(net);
use DateTime;
use DateTime::TimeZone;
use Geo::IP;

set logger => 'console';

my $gi = Geo::IP->open( "/usr/local/share/GeoIP/GeoLiteCity.dat", GEOIP_MEMORY_CACHE ) or die;

post '/' => sub {
    my $ref;
    try {
        $ref = from_json(request->body, { allow_nonref => 1 });
    }
    catch {
        send_error('Bad request', 400);
    };

    if ( ref($ref) ne "ARRAY" ) {
        $ref = [ $ref ];
    }

    my $out;
    foreach my $ip ( @{ $ref } ) {
        # Skip if this value isn't a dotted quad (e.g., "1.1.1.1")
        next if $ip !~ /^$RE{net}{IPv4}{dec}$/;
        my $record = $gi->record_by_addr($ip);
        my $country = $record->country_name;
        my $region = $record->region_name;
        $out->{$ip} = {
            country => $country,
            region => $region,
        };
        my $tz = $record->time_zone;
        if ( $tz ) {
            my $dt = DateTime->now();
            my $dtz = DateTime::TimeZone->new( name => $tz );
            $out->{$ip}->{timezone} = $tz;
            $out->{$ip}->{utc_offset} = $dtz->offset_for_datetime($dt),
        }
        else {
            $out->{$ip}->{timezone} = 'unknown';
            $out->{$ip}->{utc_offset} = 0;
        }

    }

    content_type 'application/json';
    return to_json($out);
};

start;
