/* eslint-env node */

var pm2 = require('pm2');

var instances = process.env.WEB_CONCURRENCY || -1;
  // ^ Set by Heroku or -1 to scale to max cpu core -1
var maxMemory = process.env.WEB_MEMORY || 512;

pm2.connect(function() {
  pm2.start({
    script: 'index.js',
    watch: true,
      // ^ TODO check if this is harmful in production
    exec_mode: 'cluster',
      // ^ https://github.com/Unitech/PM2/blob/master/ADVANCED_README.md#schema
    instances: instances,
    max_memory_restart: maxMemory + 'M',
      // ^ Auto restart if process taking more than XXmo
    env: {
      "NODE_ENV": "production"
    }
  }, function(err) {
    if (err) {
      console.error('Error while launching applications', err.stack || err);
      return;
    }
    console.log('PM2 and application has been succesfully started');

    // Display logs in standard output
    pm2.launchBus(function(err, bus) {
      if (err) {
        console.error('Error while launching bus', err.stack || err);
        return;
      }
      console.log('[PM2] Log streaming started');

      bus.on('log:out', function(packet) {
        var d = new Date();
        console.log('[App:%s] %s\n%s', packet.process.name, d.toISOString(), packet.data);
      });

      bus.on('log:err', function(packet) {
        var d = new Date();
        console.log('[App:%s][Err] %s\n %s', packet.process.name, d.toISOString(), packet.data);
      });
    });
  });
});
