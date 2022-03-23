cd out
for file in *.dot
do echo $file
    gvpack -u < $file | dot -Tpng -o ${file:r}.png
   rm $file
done
cd ..
