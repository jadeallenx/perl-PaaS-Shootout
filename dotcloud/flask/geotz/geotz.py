from flask import Flask, request, make_response
from pytz import timezone
from datetime import datetime
import json
import pygeoip

DEBUG=True

app = Flask(__name__)
app.config.from_object(__name__)

gi = pygeoip.GeoIP("GeoLiteCity.dat")

@app.route('/', methods=['POST'])
def lookup():
    rv = dict()
    for ip in request.json:
        r = gi.record_by_addr(ip)
        tz = timezone(r['time_zone'])
        rv[ip] = { 
            'country': r['country_name'],
#            'region': r['region'],
            'city': r['city'],
            'time_zone': r['time_zone'],
            'utc_offset': int(tz.utcoffset(datetime.utcnow()).total_seconds())
        }

    response = make_response(json.dumps(rv))
    response.headers['Content-Type'] = 'application/json'
    return response

if __name__ == '__main__':
    app.run()

