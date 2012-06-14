To deploy this in your own environment, you'll need to do something like the following

(You must also install the Heroku "Toolbelt" on your system.)

```
cp -a mojo /tmp/heroku
cd /tmp/heroku
git init
git add *
git commit -a -m'Initial commit'
heroku create --stack cedar --buildpack http://github.com/mrallen1/heroku-buildpack-perl.git
git push heroku master
```
