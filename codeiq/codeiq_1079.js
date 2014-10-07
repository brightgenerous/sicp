//# coding: utf-8

// https://codeiq.jp/ace/joboffer_apli/q1079

var datas = ['0', '5', '123', '015', '1132'];

function dividePrime(v) {
  if (v <= 0) { return false; }
  return [2, 3, 5, 7].some(function(p) {
    return v % p == 0
  })
}

var counts = datas.map(function(data) {
    var e = data.split('');
    var ops = [['']];
    for (var i = 1; i < e.length; i++) {
      ops = ops.map(function(op) {
        return [op.concat(''), op.concat('+'), op.concat('-')];
      })
      ops = Array.prototype.concat.apply([], ops);
    }
    var count = ops.map(function(op) {
        var exp = 'parseInt("';
        for (var i = 0; i < e.length; i++) {
          if (op[i] != '') {
            exp += '")' + op[i] + 'parseInt("';
          }
          exp += e[i];
        }
        exp += '")';
        return dividePrime(eval(exp));
      }).filter(function(b) { return b; }).length
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

