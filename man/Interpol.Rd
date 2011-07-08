\name{Interpol}
\alias{Interpol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolation of encoded amino acid sequences
}
\description{
Interpolation of encoded protein sequences to a specific length. Interpol can be used to transform protein sequences to uniform length for subsequent classification.
}
\usage{
Interpol(data, dims, method = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	list of encoded protein sequences (as numerical vectors)
}
  \item{dims}{
	interpolation value (desired dimensionality)
}
  \item{method}{
	"linear": linear interpolation

	"spline": cubic spline interpolation

	"natural": fulfills natural boundary conditions

	"periodic": fulfills periodic boundary conditions

	"fmm": interpolation of Forsythe
	
	"average": interval based averaging
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 returns the interpolated encoded amino acid sequences with desired dimensionality.
}
\references{
Heider D., Verheyen J., Hoffmann D.: Machine learning on normalized protein sequences, BMC Research Notes 2011, 4:94.
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

	#### V3 data
	data(V3)
	sequence = V3[1]

	#### encoding of sequence a with descriptor 151 and with normalization [0,1]
	b = AAdescriptor(sequence, 151, 2)
	b

	#### interpolation to length 27 with linear interpolation
	c = Interpol(b, 27, "linear")
	c

	#### plotting
	plot(unlist(b), type="l", col="darkgreen", ylim=c(-3,3), ylab="descriptor value", xlab="sequence position", lwd=2)
	lines(seq(1,length(unlist(b)),(length(unlist(b))/length(as.vector(c)))),as.vector(c), col="red", lwd=2)
	axis(3, at=seq(1,35, 35/27), labels=1:27)

}
