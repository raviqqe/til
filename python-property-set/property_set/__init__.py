w = 5


def c():
    return {}


def j():
    x = 850
    d = 20 * 2
    j_sc_p = 0.1758

    p = {
        "j_nk": 60,
        "j_sc": 15,
        "w_bd": 5,
        "w_ac": 15,
        "w_dc": 5,
    }

    assert sum(p.values()) == 100

    ps = {k: v / 100 * x / d for k, v in p.items()}

    return {
        "ps": ps,
        "j_sc_p": ps["j_sc"] / j_sc_p * w,
    }
