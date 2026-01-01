import seaborn
import sys
import matplotlib.pyplot
import polars
import numpy


bits = 8


def main():
    with open(sys.argv[1], "rb") as file:
        xs = numpy.array(list(file.read()))

    # Turn into a 2D array with 3 columns
    xs = numpy.stack([xs, *(xs // (1 << i) == 0 for i in range(bits))], axis=1)

    frame = polars.DataFrame(xs)

    print(frame)

    seaborn.displot(frame, x="column_0", discrete=True)
    # seaborn.displot(array, x=0, hue=2, discrete=True)

    # matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
