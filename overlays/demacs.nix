# {inputs, ...}:

# (final: prev: {
#   inherit (inputs.demacs.packages.${final.system}) demacs;
# })

# { inputs, ... }:

# (final: prev: {
#   demacs = let
#     system = final.system;
#     demacsPkg = inputs.demacs.packages.${system}.demacs;
#   in
#     demacsPkg;
# })
