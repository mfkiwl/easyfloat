import numpy as np
from pyeasyfloat.backend import *
import pyeasyfloat.backend
from pyeasyfloat.float import FloatPoint
import pyeasyfloat

def test_reciprocal(backend: BaseFPBackend, seed: int = 0):
    
    np.random.seed(0)
    input_lst = np.random.random(100)
    for x in input_lst:
        x = np.float32(x)
        fx = FloatPoint.from_bits(x.view(np.uint32), 8, 23)
        dut = backend.reciprocal(fx)
        ref = np.float32(1) / x
        np_dut = np.uint32(dut.to_bits()).view(np.float32)
        print(np_dut, ref)
    
test_reciprocal(HwBackend('MulAddExp2Rec.sv'))