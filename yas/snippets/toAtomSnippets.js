/**
 * This is an emacs yasnippets importer.
 */
"use strict";

const FS = require('fs');
const Path = require('path');


if( process.argv.length > 2 ) {
  start( process.argv[2] );
} else {
  console.error( "Missing mandatory argument: the folder where the yasnippets are." );
}

function start(folder) {
  FS.readdir( folder, (err, files) => {
    if( err ) {
      console.error( err );
      return;
    }
    files.forEach( parseFile.bind( null, folder ) );
  });
}


function parseFile( folder, filename ) {
  var filepath = Path.join( folder, filename );
  FS.readFile( filepath, (err, data) => {
      if( err ) {
        console.error( "# Unable to read file: " + filepath );
        return;
      }
      var content = data.toString();
      var lines = content.split( "\n" );
      var params = { name: removeYasExtension( filename ) };
      params.key = params.name;
      while( parseLine( lines, params ) );
      console.log( "  " + JSON.stringify( params.name ) + ":" );
      console.log( "    prefix: " + JSON.stringify( params.key ) );
      console.log( '    body: """' + lines.join("\n") + '"""' );
  });
}


function removeYasExtension( filename ) {
  var s = filename.length;
  var needle = '.yasnippet';
  if( filename.substr( s - needle.length ) == needle ) {
    return filename.substr( s - needle.length );
  }
  return filename;
}

function parseLine( lines, params ) {
  if( lines.length == 0 ) return false;
  var line = lines[0].trim();
  if( line.charAt(0) != '#' ) return false;
  // Get rid of the comment symbol.
  line = line.substr( 1 ).trim();
  lines.shift();
  if( line == '--' ) {
    // This is the end of the yasnippet header.
    return false;
  }
  var colonPosition = line.indexOf( ':' );
  if( colonPosition < 3 ) return true;
  var key = line.substr(0, colonPosition).trim().toLowerCase();
  if( key != 'key' && key != 'name' ) return true;
  var val = line.substr(colonPosition + 1).trim();
  params[key] = val;
  return true;
}
