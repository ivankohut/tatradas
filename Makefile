all : 
	echo "make console-fpc  OR  make console-dcc"

console-fpc : 
	fpc -Sd TatraDAS.dpr

console-dcc :
	cd source
	dcc -CC TatraDAS.dpr -U../third/synedit -U../third/mphexeditor
	cd ..