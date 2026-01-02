import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


bits = 8


def main():
    with open(sys.argv[1], "rb") as file:
        xs = numpy.array(list(file.read()))

    ys = numpy.stack([xs & (1 << bit) > 0 for bit in range(bits)]).transpose()
    zs = numpy.array([])

    frame = polars.DataFrame({"code": xs, "bit": ys})

    print(xs)
    print(frame)

    seaborn.displot(frame, x="code", discrete=True)
    # seaborn.displot(array, x=0, hue=2, discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
