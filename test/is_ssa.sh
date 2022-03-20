cat << EOF > pyscript.py 
import json
import sys
def is_ssa(bril):
    for func in bril["functions"]:
        assigned = set()
        for instr in func["instrs"]:
            if "dest" in instr:
                if instr["dest"] in assigned:
                    return False
                else:
                    assigned.add(instr["dest"])
    return True
if __name__ == "__main__":
    print("yes" if is_ssa(json.load(sys.stdin)) else "no")
EOF

chmod 755 pyscript.py

dune build
for f in test/benchmarks/*.json
do
    echo "$f"
    dune exec -- bin/main.exe -O gSSA < "$f" | python pyscript.py
done

rm pyscript.py
