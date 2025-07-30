from pyeasyfloat.backend import *
from pyeasyfloat.float import FloatPoint
import numpy as np

def test_reciprocal_by_div(backend: BaseFPBackend, dtype=np.float16):
    skipped_subnormals = 0
    errors = 0
    tests = 0
    match dtype:
        case np.float16:
            utype = np.uint16
            ew, mw = 5, 10
        case np.float32:
            utype = np.uint32
            ew, mw = 8, 23
        case _:
            raise ValueError(f"Unsupported dtype: {dtype}")
    one = dtype(1.0)

    for fp in FloatPoint.get_all_normal_numbers(ew, mw, postive=True, negative=True):
        tests += 1
        dut = backend.div(FloatPoint.from_numpy(one), fp)
        ref = one / fp.to_numpy()
        if dut.to_bits() != ref.view(utype):
            if FloatPoint.from_numpy(ref).is_subnormal and dut.is_zero:
                skipped_subnormals += 1
                continue
            print(f"Error in reciprocal for {fp.to_numpy()} {fp.to_bits()}: DUT={dut.to_numpy()}, REF={ref}")
            print(dut)
            print(FloatPoint.from_numpy(ref))
            errors += 1
    print(f"Skipped subnormals: {skipped_subnormals}, Errors: {errors} out of {tests} tests")

test_reciprocal_by_div(PyEasyFloatBackend(), np.float16)
test_reciprocal_by_div(HwBackend('../Div.sv'), np.float16)