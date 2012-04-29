# porter-stemmer

Porter Stemmer algorithm implementation in Clojure.

**Note**: This is my first attempt at writing Clojure. Not very pretty and
certainly not efficient.

## Usage

* Download the [sample vocabulary](http://tartarus.org/~martin/PorterStemmer/voc.txt)

* Compile & run:

        $ lein uberjar
        $ java -jar porter-stemmer-1.0.0-SNAPSHOT-standalone.jar voc.txt > stems.txt

## License

Copyright (C) 2012 Mikko Nylén

Distributed under the BSD license
