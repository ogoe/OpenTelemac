%
% LaTeX class for Telemac-Mascaret documentation
% Original authors  : Antoine Joly, Pablo Tassi
%

\ProvidesClass{TelemacDoc}[2015/11/24]

%%---------------------------------------------------------------------------
%%Options that need to be passed
%%---------------------------------------------------------------------------
%% pass options to the book class
%\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
%\ProcessOptions\relax

%----------------------------------------------------------------------------------------
%	DECLARE CLASS AND GEOMETRY
%----------------------------------------------------------------------------------------

\LoadClass[a4paper,twoside,11pt]{book}


\RequirePackage[top=3cm,bottom=3cm,left=3.2cm,right=3.2cm,headsep=10pt]{geometry} % Page margins

%----------------------------------------------------------------------------------------
%	DEFINE COLORS
%----------------------------------------------------------------------------------------
\RequirePackage{xcolor}
\definecolor{LHSVLightBlue}{cmyk}{0.99,0.29,0.00,0.12}
\definecolor{LHSVDarkBlue}{cmyk}{1.00,0.35,0.00,0.20}
\definecolor{EdfBlue}{RGB}{0,91,187}
\definecolor{EdfDBlue}{RGB}{9,53,122}
\definecolor{EdfLBlue}{RGB}{56,174,255}
\definecolor{EdfOrange}{RGB}{255,160,47}
\definecolor{EdfLOrange}{RGB}{243,157,0}
\definecolor{EdfGreen}{RGB}{80,158,47}
\definecolor{EdfDGreen}{RGB}{196,214,47}
\definecolor{PantoneRed}{RGB}{232,17,45}
\definecolor{PantonePurple}{RGB}{137,79,191}
\definecolor{PantoneGreen}{RGB}{0,153,124}
\definecolor{PantonePink}{RGB}{214,2,112}

%\colorlet{ColorBase1}{EdfBlue}

% Please refer to http://www.colorhexa.com
% or http://rgb.to/ for further details (use RGB Decimal)
\definecolor{Sand}{RGB}{194, 178, 128}
\definecolor{OceanWaveBlue}{RGB}{78, 119, 229}
\definecolor{LightOceanGreen}{RGB}{90, 225, 146}
\definecolor{BostonUniversityRed}{RGB}{204, 0, 0}
\definecolor{LightGreen}{RGB}{87, 151, 47}
\definecolor{ZinnwalditeBrown}{RGB}{44, 22, 8}
\definecolor{Peridot}{RGB}{233, 214, 0}
\definecolor{T3D}{RGB}{165, 57, 76}

%----------------------------------------------------------------------------------------
%	DEFINE CLASS OPTIONS
%----------------------------------------------------------------------------------------

\DeclareOption{Telemac2D}{\colorlet{ColorBase1}{EdfBlue}}
\DeclareOption{Telemac3D}{\colorlet{ColorBase1}{T3D}}
\DeclareOption{Sisyphe}{\colorlet{ColorBase1}{Sand}}
\DeclareOption{Tomawac}{\colorlet{ColorBase1}{OceanWaveBlue}}
\DeclareOption{Artemis}{\colorlet{ColorBase1}{LightOceanGreen}}
\DeclareOption{Postel3D}{\colorlet{ColorBase1}{BostonUniversityRed}}
\DeclareOption{Stbtel}{\colorlet{ColorBase1}{LightGreen}}
\DeclareOption{Waqtel}{\colorlet{ColorBase1}{ZinnwalditeBrown}}
\DeclareOption{Misc}{\colorlet{ColorBase1}{Peridot}}

\DeclareOption{french}{
	% changer titre de la table des matière
	\renewcommand{\contentsname}{Table des mati\`{e}res}
	%changer le titre de la liste des figures
	\renewcommand{\listfigurename}{Liste des figures}
	%changer le titre de la liste des tableaux
	\renewcommand{\listtablename}{Liste des tableaux}
	%changer la référence des tableaux
	\renewcommand*{\tablename}{Tableau}
	%changer la référence des figures
	\renewcommand*{\figurename}{Figure}
	% Chapitre
	\renewcommand*{\chaptername}{Chapitre}
	% Partie
	\renewcommand*{\partname}{Partie}
	% Annexe
	\renewcommand*{\appendixname}{Annexe}
	%références
  	%\renewcommand{\refname}{Références}
	\renewcommand{\bibname}{R\'{e}f\'{e}rences}
%	%Guillemets
	\usepackage[T1]{fontenc}
	\newcommand{\og}{\guillemotleft}
	\newcommand{\fg}{\guillemotright\xspace}
}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ProcessOptions\relax

%----------------------------------------------------------------------------------------
%	DEFINE CODE NAMES
%----------------------------------------------------------------------------------------

\newcommand{\telemacsystem}{{\scshape Telemac-Mascaret system}\xspace}
\newcommand{\telemac}[1]{{\scshape Telemac-#1}\xspace}
\newcommand{\sisyphe}{{\scshape Sisyphe}\xspace}
\newcommand{\stbtel}{{\scshape Stbtel}\xspace}
\newcommand{\tomawac}{{\scshape Tomawac}\xspace}
\newcommand{\artemis}{{\scshape Artemis}\xspace}
\newcommand{\bief}{{\scshape Bief}\xspace}
\newcommand{\damo}{{\scshape Damocles}\xspace}
\newcommand{\paral}{{\scshape Parallel}\xspace}
\newcommand{\postel}{{\scshape Postel-3D}\xspace}
\newcommand{\Special}{{\scshape Special}\xspace}
\newcommand{\mascaret}{{\scshape Mascaret}\xspace}
\newcommand{\waqtel}{{\scshape Waqtel}\xspace}
\newcommand{\tel}{{\scshape Telemac}\xspace}
\newcommand{\estel}{{\scshape Estel-3D}\xspace}

%----------------------------------------------------------------------------------------
%	VARIOUS REQUIRED PACKAGES
%----------------------------------------------------------------------------------------

%Fonts
\RequirePackage{avant} % Use the Avantgarde font for headings
\RequirePackage{mathptmx} % Use the Adobe Times Roman as the default text font together with math symbols from the Sym­bol, Chancery and Com­puter Modern fonts
%\RequirePackage{microtype} % Slightly tweak font spacing for aesthetics

%Encoding
\RequirePackage[utf8]{inputenc} % Required for including letters with accents
\RequirePackage[T1]{fontenc} % Use 8-bit encoding that has 256 glyphs

\RequirePackage[compact]{titlesec} % Allows customization of titles

\RequirePackage{graphicx} % Required for including pictures
%\graphicspath{{Pictures/}} % Specifies the directory where pictures are stored

\RequirePackage{tikz} % Required for drawing custom shapes
\usetikzlibrary{positioning,shapes.misc}
\usetikzlibrary{decorations.pathmorphing}
\usetikzlibrary{shapes,fit}
\definecolor{fbase}{gray}{0.95}
\definecolor{cPG}{RGB}{235, 227, 150}
\definecolor{link}{RGB}{150, 81, 0}
\usepackage{minibox}


\RequirePackage{eso-pic} % Required for specifying an image background in the title page

\RequirePackage{amsmath,amsfonts,amssymb,amsthm,bm} % For math equations, theorems, symbols, etc

% Figures
\RequirePackage{float}
\RequirePackage{rotating}
\RequirePackage[format=hang]{subfig}
\RequirePackage{caption}
\RequirePackage{import}


% Tables
\RequirePackage{booktabs,multirow}
\RequirePackage{lscape}
\RequirePackage{longtable}
\RequirePackage{diagbox}

% To include code
\RequirePackage{listings}


% For references
\RequirePackage[sort&compress,numbers,square]{natbib}

% For hyper links
\RequirePackage{hyperref}

% ** start PT
% Avoid ignoring all spaces after any macro, user defined or not
\RequirePackage{xspace}
% ** end PT

% To add newline after subparagraph
\RequirePackage{titlesec}

\RequirePackage{pbox}

%----------------------------------------------------------------------------------------
%	MATHEMATICAL SHORTCUT
%----------------------------------------------------------------------------------------
\input{../../data/TelemacDocMathematicalShortcut}

%----------------------------------------------------------------------------------------
% SECTION LAYOUT
%----------------------------------------------------------------------------------------
% Definition of \subparagraph starting new line after heading
\titleformat{\subparagraph}
    {\normalfont\normalsize\bfseries}{\thesubparagraph}{1em}{}
    \titlespacing*{\subparagraph}{\parindent}{3.25ex plus 1ex minus .2ex}{.75ex plus .1ex}


%----------------------------------------------------------------------------------------
%	TITLE PAGE
%----------------------------------------------------------------------------------------

%Soustitre
\def\s@btitle{\relax}
\def\subtitle#1{\gdef\s@btitle{#1}}


%version
\def\@version{\relax}
\def\version#1{\gdef\@version{#1}}

%lengths
\newlength{\TitleMargin}
\setlength{\TitleMargin}{1.5cm}

\newlength{\TitleWidth}
\setlength{\TitleWidth}{\paperwidth}
\addtolength{\TitleWidth}{-\TitleMargin}
\addtolength{\TitleWidth}{-\TitleMargin}

\newlength{\TitleHeight}
\setlength{\TitleHeight}{\paperheight}
\addtolength{\TitleHeight}{-\TitleMargin}
\addtolength{\TitleHeight}{-\TitleMargin}

\newlength{\TitleTextWidth}
\setlength{\TitleTextWidth}{\TitleWidth}
\addtolength{\TitleTextWidth}{-1cm}


% Title shape
\tikzset{
  rectangle with rounded corners north west/.initial=4pt,
  rectangle with rounded corners south west/.initial=4pt,
  rectangle with rounded corners north east/.initial=4pt,
  rectangle with rounded corners south east/.initial=4pt,
}
\makeatletter
\pgfdeclareshape{rectangle with rounded corners}{
  \inheritsavedanchors[from=rectangle] % this is nearly a rectangle
  \inheritanchorborder[from=rectangle]
  \inheritanchor[from=rectangle]{center}
  \inheritanchor[from=rectangle]{north}
  \inheritanchor[from=rectangle]{south}
  \inheritanchor[from=rectangle]{west}
  \inheritanchor[from=rectangle]{east}
  \inheritanchor[from=rectangle]{north east}
  \inheritanchor[from=rectangle]{south east}
  \inheritanchor[from=rectangle]{north west}
  \inheritanchor[from=rectangle]{south west}
  \backgroundpath{% this is new
    % store lower right in xa/ya and upper right in xb/yb
    \southwest \pgf@xa=\pgf@x \pgf@ya=\pgf@y
    \northeast \pgf@xb=\pgf@x \pgf@yb=\pgf@y
    % construct main path
    \pgfkeysgetvalue{/tikz/rectangle with rounded corners north west}{\pgf@rectc}
    \pgfsetcornersarced{\pgfpoint{\pgf@rectc}{\pgf@rectc}}
    \pgfpathmoveto{\pgfpoint{\pgf@xa}{\pgf@ya}}
    \pgfpathlineto{\pgfpoint{\pgf@xa}{\pgf@yb}}
    \pgfkeysgetvalue{/tikz/rectangle with rounded corners north east}{\pgf@rectc}
    \pgfsetcornersarced{\pgfpoint{\pgf@rectc}{\pgf@rectc}}
    \pgfpathlineto{\pgfpoint{\pgf@xb}{\pgf@yb}}
    \pgfkeysgetvalue{/tikz/rectangle with rounded corners south east}{\pgf@rectc}
    \pgfsetcornersarced{\pgfpoint{\pgf@rectc}{\pgf@rectc}}
    \pgfpathlineto{\pgfpoint{\pgf@xb}{\pgf@ya}}
    \pgfkeysgetvalue{/tikz/rectangle with rounded corners south west}{\pgf@rectc}
    \pgfsetcornersarced{\pgfpoint{\pgf@rectc}{\pgf@rectc}}
    \pgfpathclose
 }
}
\makeatother

% Make title
\makeatletter
\renewcommand{\maketitle}{
	\thispagestyle{empty}
	\begin{titlepage}
	\begin{tikzpicture}[
		remember picture,
		overlay,]
		% define top left coordinate
		\path (current page.north west) ++ (\TitleMargin,-\TitleMargin) coordinate (TitleAnchor) {};
		% Define Title page background
		\node[
			anchor=north west,
			shape=rectangle with rounded corners,
			rectangle with rounded corners north west=30pt,
			rectangle with rounded corners south west=30pt,
			rectangle with rounded corners north east=30pt,
			rectangle with rounded corners south east=0pt,
			minimum width=\TitleWidth,
			minimum height=\TitleHeight,
			fill=ColorBase1]
			(TitleBox)
			at  (TitleAnchor)
			{};
		\node[node distance= 2cm and 1cm,
			anchor=north west,
			text width=\TitleTextWidth,
			text=white]
			[below right = of TitleBox.north west]
			{
				\fontsize{72}{80}\bfseries\MakeUppercase\selectfont
				\@title
			        \vspace*{1cm}
			        \\
			        \fontsize{48}{55}\bfseries\MakeUppercase\selectfont
			        \s@btitle
			        \vspace*{1cm}
			        \\
			        \fontsize{18}{20}\bfseries\MakeUppercase\selectfont
			        \@author
			};
		\node[node distance= 2cm and 1cm,
			anchor=south west,
			text width=\TitleTextWidth,
			text=white]
			[above right = of TitleBox.south west]
			{
				\fontsize{18}{20}\bfseries\MakeUppercase\selectfont
				Version \@version
				\\
				\normalsize
				\@date
			};
		\node[node distance= 1cm,
			anchor=south east]
			[above left = of TitleBox.south east,]
			{\includegraphics[width=2cm,angle=-90]{../../data/TelemacLogoWhite.pdf}
			};
	\end{tikzpicture}
	\end{titlepage}%
}
\makeatother


%----------------------------------------------------------------------------------------
%	COPYRIGHT
%----------------------------------------------------------------------------------------

\newcommand{\TelemacCopyright}{%
\vspace*{2cm}
\begin{center}
\section*{AVERTISSEMENT / CAUTION}
\end{center}

\bigskip

L'acc\`{e}s \`{a} ce document, ainsi que son utilisation, sont strictement limit\'{e}s aux personnes express\'{e}ment habilit\'{e}es par EDF.

EDF ne pourra \^{e}tre tenu responsable, au titre d'une action en responsabilit\'{e} contractuelle, en responsabilit\'{e} d\'{e}lictuelle ou de tout autre action, de tout dommage direct ou indirect, ou de quelque nature qu'il soit, ou de tout pr\'{e}judice, notamment, de nature financier ou commercial, r\'{e}sultant de l'utilisation d'une quelconque information contenue dans ce document.

Les donn\'{e}es et informations contenues dans ce document sont fournies "en l'\'{e}tat" sans aucune garantie expresse ou tacite de quelque nature que ce soit.

Toute modification, reproduction, extraction d'\'{e}l\'{e}ments, r\'{e}utilisation de tout ou partie de ce document sans autorisation pr\'{e}alable \'{e}crite d'EDF ainsi que toute diffusion externe \`{a} EDF du pr\'{e}sent document ou des informations qu'il contient est strictement interdite sous peine de sanctions.

\bigskip

\begin{center}
\--\--\--\--\--\--\--
\end{center}

The access to this document and its use are strictly limited to the persons expressly authorized to do so by EDF.

EDF shall not be deemed liable as a consequence of any action, for any direct or indirect damage, including, among others, commercial or financial loss arising from the use of any information contained in this document.

This document and the information contained therein are provided "as are" without any warranty of any kind, either expressed or implied.

Any total or partial modification, reproduction, new use, distribution or extraction of elements of this document or its content, without the express and prior written consent of EDF is strictly forbidden. Failure to comply to the above provisions will expose to sanctions.
\clearpage
}

%----------------------------------------------------------------------------------------
%	MAIN TABLE OF CONTENTS
%----------------------------------------------------------------------------------------

\RequirePackage{titletoc} % Required for manipulating the table of contents

\contentsmargin{0cm} % Removes the default margin
% Chapter text styling
\titlecontents{chapter}[1.25cm] % Indentation
{\addvspace{15pt}\large\sffamily\bfseries} % Spacing and font options for chapters
{\color{ColorBase1}\contentslabel[\Large\thecontentslabel]{1.25cm}\color{ColorBase1}} % Chapter number
{}
{\color{ColorBase1}\normalsize\sffamily\bfseries\;\titlerule*[.5pc]{.}\;\thecontentspage} % Page number
% Section text styling
\titlecontents{section}[1.25cm] % Indentation
{\addvspace{5pt}\sffamily\bfseries} % Spacing and font options for sections
{\contentslabel[\thecontentslabel]{1.25cm}} % Section number
{}
{\sffamily\hfill\color{black}\thecontentspage} % Page number
[]
% Subsection text styling
\titlecontents{subsection}[1.25cm] % Indentation
{\addvspace{1pt}\sffamily\small} % Spacing and font options for subsections
{\contentslabel[\thecontentslabel]{1.25cm}} % Subsection number
{}
{\sffamily\;\titlerule*[.5pc]{.}\;\thecontentspage} % Page number
[]

\captionsetup{lofdepth=1}


%----------------------------------------------------------------------------------------
%	PAGE HEADERS
%----------------------------------------------------------------------------------------

\usepackage{fancyhdr} % Required for header and footer configuration

\pagestyle{fancy}
\renewcommand{\chaptermark}[1]{\markboth{\sffamily\normalsize\chaptername\ \thechapter.\ #1}{}} % Chapter text font settings
\renewcommand{\sectionmark}[1]{\markright{\sffamily\normalsize\thesection\hspace{5pt}#1}{}} % Section text font settings
\fancyhf{} \fancyhead[LE,RO]{\sffamily\normalsize\thepage} % Font setting for the page number in the header
\fancyhead[LO]{\rightmark} % Print the nearest section name on the left side of odd pages
\fancyhead[RE]{\leftmark} % Print the current chapter name on the right side of even pages
\renewcommand{\headrulewidth}{0.5pt} % Width of the rule under the header
\addtolength{\headheight}{2.5pt} % Increase the spacing around the header slightly
\renewcommand{\footrulewidth}{0pt} % Removes the rule in the footer
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}} % Style for when a plain pagestyle is specified

% Removes the header from odd empty pages at the end of chapters
\makeatletter
\renewcommand{\cleardoublepage}{
\clearpage\ifodd\c@page\else
\hbox{}
\vspace*{\fill}
\thispagestyle{empty}
\newpage
\fi}

%----------------------------------------------------------------------------------------
%	SECTION NUMBERING IN THE MARGIN
%----------------------------------------------------------------------------------------

\makeatletter
\renewcommand{\@seccntformat}[1]{\llap{\textcolor{ColorBase1}{\csname the#1\endcsname}\hspace{1em}}}
\renewcommand{\section}{\@startsection{section}{1}{\z@}
{-4ex \@plus -1ex \@minus -.4ex}
{1ex \@plus.2ex }
{\normalfont\large\sffamily\bfseries}}
\renewcommand{\subsection}{\@startsection {subsection}{2}{\z@}
{-3ex \@plus -0.1ex \@minus -.4ex}
{0.5ex \@plus.2ex }
{\normalfont\sffamily\bfseries}}
\renewcommand{\subsubsection}{\@startsection {subsubsection}{3}{\z@}
{-2ex \@plus -0.1ex \@minus -.2ex}
{.2ex \@plus.2ex }
{\normalfont\small\sffamily\bfseries}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}
{-2ex \@plus-.2ex \@minus .2ex}
{.1ex}
{\normalfont\small\sffamily\bfseries}}

%----------------------------------------------------------------------------------------
%	HYPERLINKS IN THE DOCUMENTS
%----------------------------------------------------------------------------------------

% For an unclear reason, the package should be loaded now and not later
\hypersetup{pdfborder={0 0 0},
	colorlinks=false,
	breaklinks=true,
	urlcolor=ColorBase1,
	pdftitle={Title},
	pdfauthor={Author}}


%----------------------------------------------------------------------------------------
%	CHAPTER HEADINGS
%----------------------------------------------------------------------------------------

% The set-up below should be (sadly) manually adapted to the overall margin page septup controlled by the geometry package loaded in the main.tex document. It is possible to implement below the dimensions used in the goemetry package (top,bottom,left,right)... TO BE DONE


\RequirePackage{varwidth} %to fix a mximum width for the chapter text

%lengths
\newlength{\ChapterOffsetx}
\setlength{\ChapterOffsetx}{3.2cm}
\newlength{\ChapterOffsety}
\setlength{\ChapterOffsety}{6cm}

\newlength{\ChapterBoxWidth}
\setlength{\ChapterBoxWidth}{\paperwidth}
\addtolength{\ChapterBoxWidth}{-\ChapterOffsetx}
\addtolength{\ChapterBoxWidth}{-\ChapterOffsetx}

\newlength{\ChapterTextWidth}
\setlength{\ChapterTextWidth}{\ChapterBoxWidth}
\addtolength{\ChapterTextWidth}{-1cm}

% Numbered chapters with mini tableofcontents
\def\thechapter{\arabic{chapter}}
\def\@makechapterhead#1{
\thispagestyle{empty}
{\centering \normalfont\sffamily
\ifnum \c@secnumdepth >\m@ne
\if@mainmatter
\startcontents
\begin{tikzpicture}[remember picture,overlay]
	\node at (current page.north west)
	{\begin{tikzpicture}[remember picture,overlay]
		\path (current page.north west) ++ (\ChapterOffsetx,-\ChapterOffsety)
		node[anchor=north west,
		shape=rectangle with rounded corners,
		rectangle with rounded corners north west=10pt,
		rectangle with rounded corners south west=10pt,
		rectangle with rounded corners north east=10pt,
		rectangle with rounded corners south east=0pt,
		minimum width=\ChapterBoxWidth,
		text width=\ChapterTextWidth,
		text=black,
		align=left,
		draw=ColorBase1,
		line width=2pt,
		fill=ColorBase1!50,
		inner sep=12pt]
		(ChapterBox)
		{\huge\sffamily\bfseries
			\thechapter. #1
%			\\
%			\vspace*{1cm}\mbox{}
		};
%		\node[node distance= 0.1cm,
%			anchor=south east]
%			[above left = of ChapterBox.south east,]
%			{\includegraphics[width=1cm,angle=-90]{TelemacLogoWhite.pdf}
%			};
	\end{tikzpicture}
	};
\end{tikzpicture}}
%\par\vspace*{150\p@}
\par\vspace*{200\p@}
\fi
\fi}

% Unnumbered chapters without mini tableofcontents (could be added though)
\def\@makeschapterhead#1{
\thispagestyle{empty}
{\centering \normalfont\sffamily
\ifnum \c@secnumdepth >\m@ne
\if@mainmatter
\begin{tikzpicture}[remember picture,overlay]
	\node at (current page.north west)
	{\begin{tikzpicture}[remember picture,overlay]
		\path (current page.north west) ++ (\ChapterOffsetx,-\ChapterOffsety)
		node[anchor=north west,
		shape=rectangle with rounded corners,
		rectangle with rounded corners north west=10pt,
		rectangle with rounded corners south west=10pt,
		rectangle with rounded corners north east=10pt,
		rectangle with rounded corners south east=0pt,
		minimum width=\ChapterBoxWidth,
		text width=\ChapterTextWidth,
		text=black,
		align=left,
		draw=ColorBase1,
		line width=2pt,
		fill=ColorBase1!50,
		inner sep=12pt]
		(ChapterBox)
		{\huge\sffamily\bfseries
			#1
%			\\
%			\vspace*{1cm}\mbox{}
		};
%		\node[node distance= 0.1cm,
%			anchor=south east]
%			[above left = of ChapterBox.south east,]
%			{\includegraphics[width=1cm,angle=-90]{TelemacLogoWhite.pdf}
%			};
	\end{tikzpicture}
	};
\end{tikzpicture}}
%\par\vspace*{100\p@}
\par\vspace*{150\p@}
\fi
\fi
}
\makeatother

%----------------------------------------------------------------------------------------
%	BIBLIOGRAPHY HEADING
%----------------------------------------------------------------------------------------

% The set-up below is used to mark the bibliography

\makeatletter
\renewenvironment{thebibliography}[1]
     {\@mkboth{\sffamily\normalsize\bibname}{\sffamily\normalsize\bibname}
     \chapter*{\bibname
        \addcontentsline{toc}{chapter}{\texorpdfstring{\color{ColorBase1}\bibname}{Bibliography}}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\makeatother

%----------------------------------------------------------------------------------------
%	TABLE SETUP
%----------------------------------------------------------------------------------------

% used to go over two lines
\newcommand{\minitab}[2][1]{\begin{tabular}{#1}#2\end{tabular}}

%----------------------------------------------------------------------------------------
%	GRAPHICS
%----------------------------------------------------------------------------------------
  \newcommand{\includegraphicsmaybe}[2] {\IfFileExists{#2} {\includegraphics#1{#2}}%
  { \includegraphics#1{../../../../documentation/data/image_not_found.pdf}%
    \newline \small{\path{#2} is missing} }}

%----------------------------------------------------------------------------------------
%	CODE DEFINITION
%----------------------------------------------------------------------------------------
\renewcommand{\ttdefault}{pcr} %to allow \ttfamily and \bfseries

% Citing a file name
\newcommand{\telfile}[1]{\textbf{#1}}
% Citing a telmac-mascaret keyword
\newcommand{\telkey}[1]{\texttt{#1}}

% Sets for listings package
\definecolor{green}{rgb}{0,0.74,0}
\definecolor{turquoise}{cmyk}{0.49,0,0,0.29}
\definecolor{ListingBackground}{rgb}{0.95,0.95,0.95}
\definecolor{purple}{cmyk}{0.45,0.86,0,0}

\lstloadlanguages{Fortran,bash,csh}

%\lstset{language=[08]Fortran,
% ** PT **
\lstdefinelanguage{TelFortran}{
  language=[90]Fortran,
  basicstyle=\ttfamily\small,
  frameround=ffff,
  stringstyle={\color{magenta}},
  showstringspaces=false,
  morekeywords={USE,PRECISION,INTENT,IN,OUT,INOUT,ONLY,ADVANCE,WHILE,POINTER,MODULE},
  deletekeywords=[1]{REAL},
  keywordstyle=\bfseries,
  commentstyle=\color{gray},
  morecomment=[l]{!},
  morekeywords=[2]{SQRT,REAL,INT,MAX,MIN},
  keywordstyle=[2]{\bfseries\color{violet}},
  morekeywords=[3]{ALLOCATED,PRESENT,DEALLOCATE,ALLOCATE},
  keywordstyle=[3]{\bfseries\itshape\color{blue}},
  literate={%
    *{.EQ.}{{{\bfseries\color{green}.EQ.}}}{4}
     {.NE.}{{{\bfseries\color{green}.NE.}}}{4}
     {.LE.}{{{\bfseries\color{green}.LE.}}}{4}
     {.LT.}{{{\bfseries\color{green}.LT.}}}{4}
     {.GE.}{{{\bfseries\color{green}.GE.}}}{4}
     {.GT.}{{{\bfseries\color{green}.GT.}}}{4}
     {.NOT.}{{{\bfseries\color{green}.NOT.}}}{5}
     {.OR.}{{{\bfseries\color{green}.OR.}}}{4}
     {.AND.}{{{\bfseries\color{green}.AND.}}}{5}
     {.TRUE.}{{{\bfseries\color{violet}.TRUE.}}}{6}
     {.FALSE.}{{{\bfseries\color{violet}.FALSE.}}}{7}
	},
    escapeinside={(*@}{@*)},
}

\lstset{backgroundcolor=\color{ListingBackground}}
%\lstset{keywordstyle=\color{purple}\bfseries\emph}
%\lstset{commentstyle=\color{blue}\itshape}
\lstset{escapeinside={(*@}{@*)}}

\lstdefinelanguage{TelemacCas}{
  basicstyle=\ttfamily\footnotesize,
  commentstyle=\color{gray},
  morecomment=[f]{/},
}


%----------------------------------------------------------------------------------------
%	BLOCKS
%----------------------------------------------------------------------------------------

% Blocks are used to emphasize certain text

%lengths
\newlength{\BlockBoxWidth}
\setlength{\BlockBoxWidth}{\textwidth}
\addtolength{\BlockBoxWidth}{-40pt}


\newlength{\BlockTextWidth}
\setlength{\BlockTextWidth}{\BlockBoxWidth}
\addtolength{\BlockTextWidth}{-40pt}

% text command
\newcommand{\BlockTitle}{}


\newenvironment{CommentBlock}[1]
{%before
\renewcommand{\BlockTitle}{#1}
\lstset{backgroundcolor=\color{EdfLBlue!20}}
\vspace*{6pt}
\begin{tikzpicture}
\path (0,0) node [
	draw=EdfLBlue,
	fill=EdfLBlue!20,
	very thick,
	rectangle,
	rounded corners,
	minimum width=\BlockBoxWidth,
	text width=\BlockTextWidth,
	inner sep=10pt,% inner ysep=10pt
	]
	(box)
		\bgroup
}
{%afer
\egroup;
\path (box.north west) node[
	fill=EdfLBlue,
	text=white,
	right=10pt] {\BlockTitle};
\end{tikzpicture}%
\lstset{backgroundcolor=\color{ListingBackground}}
}

\newenvironment{WarningBlock}[1]
{%before
\renewcommand{\BlockTitle}{#1}
\lstset{backgroundcolor=\color{PantoneRed!20}}
\vspace*{6pt}
\begin{tikzpicture}
\path (0,0) node [
	draw=PantoneRed,
	fill=PantoneRed!20,
	very thick,
	rectangle,
	rounded corners,
	minimum width=\BlockBoxWidth,
	text width=\BlockTextWidth,
	inner sep=10pt,% inner ysep=10pt
	]
	(box)
		\bgroup
}
{%afer
\egroup;
\path (box.north west) node[
	fill=PantoneRed,
	text=white,
	right=10pt] {\BlockTitle};
\end{tikzpicture}%
\lstset{backgroundcolor=\color{ListingBackground}}
}


%----------------------------------------------------------------------------------------
%	INDEX
%----------------------------------------------------------------------------------------
\RequirePackage{makeidx}
\makeindex

%\usepackage[nottoc]{tocbibind}

%add index to table of content and header
\makeatletter
\renewenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \twocolumn[
					\phantomsection
                	\@makeschapterhead{
               	 %\chapter*{
                		\indexname
                		\addcontentsline{toc}{chapter}{\texorpdfstring{\color{ColorBase1}\indexname}{Index}}
	                }
                ]%
                \@mkboth{\sffamily\normalsize\indexname}%
                        {\sffamily\normalsize\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \columnseprule \z@
                \columnsep 35\p@
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\makeatother

%----------------------------------------------------------------------------------------
% LAYOUT
%----------------------------------------------------------------------------------------
\setlength{\parindent}{0pt}
