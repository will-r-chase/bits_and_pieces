var url ='https://www.instagram.com/p/BwmMnEvAyz4/';
var page = new WebPage();
var fs = require('fs');
var outpath ='4_html.html';

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write(outpath, page.content, 'w');
            phantom.exit();
    }, 2500);
}

