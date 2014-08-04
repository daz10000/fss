if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` version#"
  exit 1
fi

cd ..
version=$1
cp -rf fss fss-${version}
rm -rf fss-${version}/*/obj
for a in `find fss-${version} -name ".svn"` ; do echo $a; rm -rf $a ; done
zip -r fss-${version}.zip fss-${version}
chmod a+r fss-${version}.zip
#scp fss-${version}.zip daz@combiol.org:/www/htdocs/fss
scp fss-${version}.zip daz@feb17.org:/var/www/html/fss
rm -rf fss-${version}
rm -rf fss-${version}.zip
