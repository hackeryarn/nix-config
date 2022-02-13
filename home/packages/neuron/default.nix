(let
  neuronRev = "6a1387113e56f4885933b10bd671e9eb25786dd2";
  neuronSrc = builtins.fetchTarball
    "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz";
  neuronPkg = import neuronSrc;
in neuronPkg.default)
