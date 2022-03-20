for i in {0..100}
do MINE=$(dune exec -- bin/main.exe -O gSSA < test/benchmarks/perfect.json | brili $i)
   ORACLE=$(brili $i < test/benchmarks/perfect.json)
   printf "%d\t%d\n" $MINE $ORACLE
done
