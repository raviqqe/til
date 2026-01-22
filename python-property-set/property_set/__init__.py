from pprint import pprint


def c():
    return {}


def j():
    x = 850
    d = 20 * 3

    p = {
        "j_nk": 60,
        "j_sc": 15,
        "w_bd": 5,
        "w_ac": 15,
        "w_dc": 5,
    }

    assert sum(p.values()) == 100

    return {k: v / 100 * x / d for k, v in p.items()}
