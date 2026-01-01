import seaborn
import sys
import polars
import matplotlib.pyplot


def main():
    with open(sys.argv[1], "rb") as file:
        code = polars.DataFrame({"code": list(file.read())})

    seaborn.displot(code, x="code")

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
