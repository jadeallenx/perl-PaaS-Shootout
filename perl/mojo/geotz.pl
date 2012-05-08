use Mojolicious::Lite;
use Mojo::JSON;

use Try::Tiny;
use Regexp::Common qw(net);
use DateTime;
use DateTime::TimeZone;
use Geo::IP;

my $gi = Geo::IP->open( "/usr/local/share/GeoIP/GeoLiteCity.dat", GEOIP_MEMORY_CACHE ) or die;
my $json = Mojo::JSON->new();

post '/' => sub {
    my $self = shift;

    my $ref = $json->decode($self->req->body);

    if ( ref($ref) ne "ARRAY" ) {
        $ref = [ $ref ];
    }

    my $out;
    foreach my $ip ( @{ $ref } ) {
        # Skip if this value isn't a dotted quad (e.g., "1.1.1.1")
        next if $ip !~ /^$RE{net}{IPv4}{dec}$/;
        my $record = $gi->record_by_addr($ip);
        $out->{$ip} = {
            country => $record->country_name,
            region => $record->region_name,
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

    return $self->render_json($out);

};

app->start;
