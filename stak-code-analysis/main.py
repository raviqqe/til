from typing import Sequence
import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


def compression(code: Sequence[int]) -> None:
    frame = polars.DataFrame(
        schema={
            "offset": polars.Int64,
            "length": polars.Int64,
        },
    )

    length = None

    for byte in code:
        if length is not None:
            frame.extend(polars.DataFrame([{"offset": byte, "length": length + 1}]))
            length = None
        elif byte & 1 == 1:
            length = byte >> 1

    seaborn.displot(frame, x="offset", discrete=True)
    seaborn.displot(frame, x="length", discrete=True)


def main() -> None:
    with open(sys.argv[1], "rb") as file:
        xs = numpy.array(list(file.read()))

    frame = polars.DataFrame(
        numpy.array([[x, i, x & (1 << i) > 0] for x in xs for i in range(8)]),
        schema=["code", "bit", "on"],
    )

    print(frame)

    seaborn.displot(frame, x="code", discrete=True)
    seaborn.displot(
        frame,
        x="bit",
        hue="on",
        hue_order=[1, 0],
        discrete=True,
        multiple="stack",
    )

    compression(xs)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
