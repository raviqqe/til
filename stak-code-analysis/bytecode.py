import seaborn
import sys
import os
import pandas
import matplotlib.pyplot


def main():
    code = os.read(sys.argv[1])
    print(code)

    seaborn.displot(code, x="code")

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
