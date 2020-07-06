var app = require('http').createServer(handler);
var url = require('url') ;

var statusCode = 200;

const args = process.argv.slice(2);
const port = args[0];

app.listen(port);

function handler (req, res) {
  var data = '';

  var queryObject = url.parse(req.url, true).query;

  if(req.method == 'GET') {
    console.log('GET')
    req.on('data', function(chunk) {
      data+=chunk;
    });

    req.on('end', function() {

    });
  }

  if (req.method == "POST") {
    console.log('POST');
    req.on('data', function(chunk) {
      data += chunk;
    });

    req.on('end', function() {
      console.log('Received body data:');
      console.log(data.toString());

    });
  }

  console.log("Query strings: " + JSON.stringify(queryObject));

  res.writeHead(statusCode, {'Content-Type': 'text/plain'});
  res.end();
}

console.log(`Listening to port ${port}`);
console.log("Returning status code " + statusCode.toString());