find . -name "*.hs" | xargs graphmod --no-cluster -q | dot -Tpng -o mods.png -Gdpi=500
