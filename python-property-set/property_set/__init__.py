# spell-checker: ignore veqt

w = 5


def c():
    x = 100_000
    d = 20

    p = {
        "veqt.to": 50,
        "tec.to": 30,
        "qqc.to": 15,
        "cash.to": 5,
    }

    assert sum(p.values()) == 100

    ps = {k: v / 100 * x for k, v in p.items()}

    return {
        "t": ps,
        "d": {k: v / d for k, v in ps.items()},
    }


def j():
    x = 850
    d = 20 * 2
    j_sc_p = 0.1758

    p = {
        "j_nk": 60,
        "j_sc": 15,
        "w_ac": 15,
        "w_dc": 5,
        "w_bd": 5,
    }

    assert sum(p.values()) == 100

    ps = {k: v / 100 * x for k, v in p.items()}

    return {
        "d": {k: v / d for k, v in ps.items()},
        "t": ps,
        "z": {
            "j_sc_p": ps["j_sc"] / j_sc_p * w,
        },
    }
