import os

def test(f):
  os.system("raco test " + f + ".rkt")
  
# test("check-values-lang-test")

# test("uniquify-test")
# test("sequentialize-let-test")
# test("select-instructions-test")
# test("impose-calling-conventions-test")
# test("canonicalize-bind-test")
# test("uncover-locals-test")
# test("undead-analysis-test")
# test("conflict-analysis-test")
# test("replace-locations-test")
# test("expose-basic-blocks-test")

test("remove-complex-opera-test")
test("specify-representation-test")

# test("assign-call-undead-variables-test")
# test("allocate-frames-test")
# test("assign-registers-test")
# test("assign-frame-variables-test")

# test("../compiler")