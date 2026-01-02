import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


bits = 8


def main():
    with open(sys.argv[1], "rb") as file:
        xs = numpy.array(list(file.read()))

    zs = numpy.array([[x, i, x & (1 << i) > 0] for x in xs for i in range(bits)])

    frame = polars.DataFrame(zs)

    print(xs)
    print(frame)

    seaborn.displot(frame, x="code", discrete=True)
    # seaborn.displot(array, x=0, hue=2, discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
