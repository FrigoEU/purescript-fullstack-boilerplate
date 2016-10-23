// module Server.Main
/* eslint-env node */

exports.morgan = function(str){
  return require("morgan")(str);
};
