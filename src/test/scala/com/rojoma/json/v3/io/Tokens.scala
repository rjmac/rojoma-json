package com.rojoma.json.v3
package io

object Tokens {
  def tokenOpenBracket() = TokenOpenBracket()(Position.Invalid)
  def tokenCloseBracket() = TokenCloseBracket()(Position.Invalid)
  def tokenOpenBrace() = TokenOpenBrace()(Position.Invalid)
  def tokenCloseBrace() = TokenCloseBrace()(Position.Invalid)
  def tokenString(s: String) = TokenString(s)(Position.Invalid)
  def tokenIdentifier(s: String) = TokenIdentifier(s)(Position.Invalid)
  def tokenNumber(s: String) = TokenNumber(s)(Position.Invalid)
  def tokenNumber(i: Int) = TokenNumber(i.toString)(Position.Invalid)
  def tokenColon() = TokenColon()(Position.Invalid)
  def tokenComma() = TokenComma()(Position.Invalid)
}
