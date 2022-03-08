for i in ../../../bril/benchmarks/*.bril
do
    FILE="$(basename "$i")"
    echo "${FILE}"
    (bril2json < $i) > "${FILE/%bril/json}"
done
