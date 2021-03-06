(define *logpro_style.css* #<<EOF
/*
 * Specify colors, font changes, etc here for the different rule classes
 */

.required, .abort, .check, .error, .warning, .ignore, .note, .trigger {
    font-weight: bold;
    background-color: whitesmoke;
}

.required {
    color: darkolivegreen;
}

.abort, .check {
    color: darkred;
}

.error {
    color: crimson;
}

.warning {
    color: goldenrod;
}

.ignore, .note {
    color: yellowgreen;
}

.trigger {
    color: navy;
}


/* 
 * Body Settings
 */

body, div, span, h1, h2, h3, h4 {
    background-color: #FFF;
    font-family: Courier, Serif;
}

a {
    color: blue;
}

a:link {
    text-decoration: underline;
}


a:active, a:hover {
    font-style: italic;
}

.exitcode {
    color: cornflowerblue;
    font-weight: bold;
    font-size: 14pt;
    font-family: Arial, Helvetica, Sans;
}

/*
 * Summary Table Settings
 */

tr:nth-child(even) {
    background-color: #FFF;
}

tr:nth-child(odd) {
    background-color: whitesmoke;
}

td, th {
    font-family: Arial, Helvetica, Sans;

    padding: 5px;
    spacing: 2px;
    align: top;
    text-align: left;


	-moz-border-radius-topright:3px;
	-webkit-border-top-right-radius:3px;
	border-top-right-radius:3px;
}
EOF
)
