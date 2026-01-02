import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


def main():
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

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
