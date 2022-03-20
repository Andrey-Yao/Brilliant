cd out
for f in *.dot
do gvpack -u < "$f" | dot -Tpng -o "${file:r}".png
   rm "$f"
done
cd ..
