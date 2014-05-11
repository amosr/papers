for x in *.c
do
	y=`basename "$x" .c`
	gcc -o out/$y-O3 $x -O3
	gcc -o out/$y-O0 $x -O0
done
