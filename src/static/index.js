// pull in desired CSS/SASS files
require( './styles/main.scss' );
//require( './font-awesome-4.7.0/scss/font-awesome.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.embed( document.getElementById( 'main' ) );
