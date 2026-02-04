# spell-checker: ignore veqt

w = 5

j_sc_p = 0.1758


def c(x=100_000):
    d = 20

    p = {
        "veqt.to": 60,
        "tec.to": 20,
        "qqc.to": 20,
    }

    assert sum(p.values()) == 100

    ps = {k: v / 100 * x for k, v in p.items()}

    return {
        "total": ps,
        "day": {k: v / d for k, v in ps.items()},
    }


def j(x=850):
    d = 20 * 2

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
        "day": {k: v / d for k, v in ps.items()},
        "total": ps,
        "week_shares": {
            "j_sc": ps["j_sc"] / j_sc_p / d * w,
        },
    }
