import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


def code(xs: numpy.ndarray) -> None:
    frame = polars.DataFrame(
        [[x, i, x & (1 << i) > 0] for x in xs for i in range(8)],
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


def compression(xs: numpy.ndarray) -> None:
    offsets = []
    lengths = []
    length: int | None = None

    for byte in xs:
        if length is not None:
            offsets.append(byte)
            lengths.append(length + 1)
            length = None
        elif byte & 1 == 1:
            length = byte >> 1

    frame = polars.DataFrame({"offset": offsets, "length": lengths})

    print(frame)

    seaborn.displot(frame, x="offset", discrete=True)
    seaborn.displot(frame, x="length", discrete=True)


def main() -> None:
    with open(sys.argv[1], "rb") as file:
        xs = numpy.array(list(file.read()))

    code(xs)
    compression(xs)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
