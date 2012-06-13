#!/bin/sh

curl -s -O http://www.cpan.org/authors/id/B/BO/BORISZ/Geo-IP-1.40.tar.gz
tar -zxf Geo-IP-1.40.tar.gz
cd Geo-IP-1.40
perl Makefile.PL PP=1
make
make install

exit 0


