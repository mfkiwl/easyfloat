from pyeasyfloat.float import *
from pyeasyfloat.fma import *

def div(x: FloatPoint, y: FloatPoint) -> FloatPoint:
    """
    return x / y
    """
    assert x.ew == y.ew and x.mw == y.mw, "x and y must have the same ew and mw"
    raw_x = x.to_raw()
    raw_y = y.to_raw()

    raw_res = RawFloatPoint()
    raw_res.sign = raw_x.sign ^ raw_y.sign

    raw_res.is_nan = raw_x.is_nan or raw_y.is_nan or (raw_x.is_inf and raw_y.is_inf)
    raw_res.is_inf = y.is_subnormal or (raw_y.is_zero and not raw_x.is_zero)
    raw_res.is_zero = raw_x.is_zero or raw_y.is_inf

    raw_res.exp = raw_x.exp - raw_y.exp

    n_pad_bits = 2 if raw_x.mantissa.bit_length() % 2 == 0 else 3

    reminder = raw_x.mantissa
    divisor = raw_y.mantissa
    quotient = 0
    first_q_is_zero = False
    quotient_bits = raw_x.mantissa.bit_length() + n_pad_bits
    for i in range(quotient_bits):
        if reminder >= divisor:
            reminder -= divisor
            q = 1
        else:
            q = 0
            if i == 0:
                first_q_is_zero = True
        reminder <<= 1
        quotient = (quotient << 1) | q
    if first_q_is_zero:
        quotient <<= 1
        raw_res.exp -= 1
        assert quotient.bit_length() == quotient_bits, f'{quotient.bit_length()} != {quotient_bits}'
    sticky_bit = reminder != 0
    raw_res.mantissa = (quotient << 1) | sticky_bit
    res = round_raw_float(raw_res, x.ew, x.mw)
    return res
