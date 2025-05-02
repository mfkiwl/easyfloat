import numpy as np
from pyeasyfloat.backend import *
import pyeasyfloat.backend
from pyeasyfloat.float import FloatPoint
import pyeasyfloat

def test_exp2(backend: BaseFPBackend, seed: int = 0):
    input_lst = [-1000, -100, -21.23, -10.38521, -4, -3, -2, -1, -0.5, 0]
    for x in input_lst:
        x = np.float32(x)
        fx = FloatPoint.from_bits(x.view(np.uint32), 8, 23)
        dut = backend.exp2(fx, 8, 23)
        ref = np.exp2(x, dtype=np.float32)
        np_dut = np.uint32(dut.to_bits()).view(np.float32)
        print(np_dut, ref)
    
# test_exp2(HwBackend('MulAddExp2.sv'))
test_exp2(PyEasyFloatBackend())