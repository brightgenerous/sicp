#! /usr/bin/ruby

#
# Ruby 2.1
#
# https://codeiq.jp/ace/techfirm_question/q1090

#
#【条件】
#  ・ライセンスコードは、入力した時点で妥当性がチェック出来ること。
#  ・コード長は、16桁の英数字の半角文字羅列とします。英字の大小は同一として扱います。
#  ・ライセンスコードは、不正防止を考慮し簡単に推測出来ないように考慮すること。
#
# 追加条件
#  ・ライセンスを追加で多数発行可能であること
#
# 設計
#  ・eval 可能な文字列を DES で暗号化する
#


require 'openssl'

class License

  @@key = "hogefoobar"

  def self.make value, key = @@key
    encrypt key, value
  end

  def self.ok? value, key = @@key
    val = decrypt key, value
    !! proc { $SAFE = 4; eval(val) rescue nil }.call if val
  end

  private

  def self.encrypt pass, value
    enc = OpenSSL::Cipher::DES.new
    enc.encrypt
    enc.pkcs5_keyivgen pass
    (enc.update(value) + enc.final).unpack("H*")[0]
  end

  def self.decrypt pass, encrypted
    dec = OpenSSL::Cipher::DES.new
    dec.decrypt
    dec.pkcs5_keyivgen pass
    begin
      dec.update([encrypted].pack("H*")) + dec.final
    rescue ArgumentError
    rescue OpenSSL::Cipher::CipherError
    end
  end
end

datas = [
  "true",    # true
  "false",   # false
  "nil",     # false
  "! true",  # false
  "! false", # true
  "! nil",   # true
  "not 1",   # false
  "not nil", # true
  "hoge",    # false
  "'hoge'",  # true
  "1+2+3",   # true
  "1.to_s",  # true
  "1 / 0"    # false
]

datas.lazy.map { |v| [v, License.make(v)] }
 .map { |v, e| [v, e, e.size, License.ok?(e)] }
 .each { |a| p a }

