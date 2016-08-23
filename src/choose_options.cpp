#include <Rcpp.h>
using namespace Rcpp;

//' (choose from a number of pre-sorted options)
//' takes a vector pair of toki1 / toki2 and a vector pair of res_token_i_1 /
//' res_token_i_2 and chooses so that each 1st and exh 2nd value only is used
//' where res_token_i_x identiefies already used items.
//' @param toki1 first number of number pair to choose from
//' @param toki2 second number of number pair to choose from
//' @param res_token_i_1 already used first numbers
//' @param res_token_i_2 already used second numbers
//' // @keywords internal
// [[Rcpp::export]]
List choose_options(
  NumericVector toki1,
  NumericVector toki2,
  NumericVector res_token_i_1,
  NumericVector res_token_i_2
)
{
  // making sets for already used tokens
  std::set<int> used1;
  for ( int i = 0; i < res_token_i_1.size() ; ++i){ used1.insert(res_token_i_1[i]); }
  std::set<int> used2;
  for ( int i = 0; i < res_token_i_2.size() ; ++i){ used2.insert(res_token_i_2[i]); }

  // storing results
  std::vector<int> tok_1(toki1.size());
  std::vector<int> tok_2(toki2.size());

  // going through all options and choose
  int v = 0;
  for ( int i = 0; i < toki1.size() ; ++i){
    if( used1.find( toki1[i] ) == used1.end() && used2.find( toki2[i] ) == used2.end() ){
      tok_1[v] = toki1[i] ;
      tok_2[v] = toki2[i] ;
      used1.insert(toki1[i]);
      used2.insert(toki2[i]);
      v = v + 1 ;
    }
  }
  std::vector<int> tok_out_1(tok_1.begin (), tok_1.begin () + v);
  std::vector<int> tok_out_2(tok_2.begin (), tok_2.begin () + v);

  // return
  return List::create(
    Named("token_i_1", tok_out_1),
    Named("token_i_2", tok_out_2)
  );
}
