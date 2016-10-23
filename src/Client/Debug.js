// module Client.Debug

exports.debug = function debug(a){
  console.log(JSON.stringify(a));
  return a;
};
