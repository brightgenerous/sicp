//# coding: utf-8

// https://codeiq.jp/ace/joboffer_apli/q1079

var datas = ['0', '5', '123', '015', '1132'];

function dividePrime(v) {
  if (v <= 0) { return false; }
  var primes = [2, 3, 5, 7];
  for (var i = 0; i < primes.length; i++) {
    if ((v % primes[i]) == 0) { return true; }
  }
  return false;
}

var counts = datas.map(function(data) {
    e = data.split('');
    var ops = [['']];
    for (var i = 1; i < e.length; i++) {
      ops = ops.map(function(op) {
        return [op.concat(''), op.concat('+'), op.concat('-')];
      })
      ops = Array.prototype.concat.apply([], ops);
    }
    var count = 0;
    ops.forEach(function(op) {
      var exp = 'parseInt("';
      for (var i = 0; i < e.length; i++) {
        if (op[i] != '') {
          exp += '")';
          exp += op[i];
          exp += 'parseInt("';
        }
        exp += e[i];
      }
      exp += '")';
      if (dividePrime(eval(exp))) { count++; }
    });
    return [data, count];
  })

/*
[ '0',    0  ],
[ '5',    1  ],
[ '123',  6  ],
[ '015',  5  ],
[ '1132', 15 ]
*/
console.log(counts);

