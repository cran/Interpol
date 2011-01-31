\name{AAdescriptor}
\alias{AAdescriptor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AAdescriptor: Descriptor encoding of amino acids
}
\description{
Encodes amino acids into numerical values using descriptors.
}
\usage{
AAdescriptor(data, descriptor = 151, normalize = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	amino acid sequence (as characters)
}
  \item{descriptor}{
	descriptor to be used (range 1-532)
}
  \item{normalize}{
	0: no normalization; 1:[-1,1]; 2:[0,1]
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 returns the encoded (and normalized) amino acid sequence.
}
\references{
	Heider D., Hauke S., Pyka M., Kessler D. Advances and Applications in Bioinformatics and Chemistry 2010, 3:15-24

	Kawashima, S.; Ogata, H.; Kanehisa, M. Nucleic Acids Res 1999, 27:368.
}
\author{
	Dominik Heider, PhD, University of Duisburg-Essen, Germany
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	#### Amino acid sequence
	a = "MEGHIK"

	#### encoding of sequence a with descriptor 151 and without normalization
	b = AAdescriptor(a, 151, 0)
}
