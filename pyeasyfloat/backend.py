from abc import ABC
from pyeasyfloat.fma import fma
from pyeasyfloat.exp import pow2
from pyeasyfloat.float import FloatPoint
from pyverilator import PyVerilator

class BaseFPBackend(ABC):
    def fma(self, a: FloatPoint, b: FloatPoint, c: FloatPoint) -> FloatPoint:
        """return a * b + c in precision of c"""
        pass
    
    def exp2(self, x: FloatPoint, targetExpWidth: int, targetMantissaWidth: int) -> FloatPoint:
        pass


class PyEasyFloatBackend(BaseFPBackend):

    def __init__(self):
        super().__init__()

    def fma(self, a: FloatPoint, b: FloatPoint, c: FloatPoint) -> FloatPoint:
        """return a * b + c in precision of c"""
        return fma(a, b, c, c.ew, c.mw)
    
    def exp2(self, x: FloatPoint, targetExpWidth: int, targetMantissaWidth: int) -> FloatPoint:
        return pow2(x, targetExpWidth, targetMantissaWidth)

class HwBackend(BaseFPBackend):

    def __init__(self, svTopFile: str):
        super().__init__()
        self.sim = PyVerilator.build(svTopFile)
    

    def fma(self, a: FloatPoint, b: FloatPoint, c: FloatPoint) -> FloatPoint:
        self.sim.io.io_in_exp2 = 0
        self.sim.io.io_in_a = a.to_bits()
        self.sim.io.io_in_b = b.to_bits()
        self.sim.io.io_in_c = c.to_bits()
        ret = self.sim.io.io_out
        return FloatPoint.from_bits(ret, c.ew, c.mw)

    def exp2(self, x: FloatPoint, targetExpWidth: int, targetMantissaWidth: int) -> FloatPoint:
        self.sim.io.io_in_exp2 = 1
        self.sim.io.io_in_a = x.to_bits()
        self.sim.io.io_in_b = 0
        self.sim.io.io_in_c = 0
        ret = self.sim.io.io_out
        # self.sim.clock.tick()
        return FloatPoint.from_bits(ret, targetExpWidth, targetMantissaWidth)