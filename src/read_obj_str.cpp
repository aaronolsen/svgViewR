#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List read_obj_str( std::vector< std::string > string) {

	// Get number of strings
	int str_size = string.size();
	
	// Initiate variables to be used
	std::string str_char;
	bool char0_s = FALSE;
	bool char1_b = FALSE;
	bool char1_f = FALSE;
	bool char1_l = FALSE;
	bool char1_v = FALSE;
	bool char2_n = FALSE;
	int	str_type = 0;
	std::string f_string;
	std::string l_string;
	std::string v_string;
	std::string vn_string;
	std::vector<double> f_vec;
	std::vector<double> l_vec;
	std::vector<double> v_vec;
	std::vector<double> vn_vec;
	
	// For each string
	for( int i=0; i < str_size; i++ ){

		// Number of characters in string
		int n_char = string[i].length();
//		Rcpp::Rcout << string[i].substr( 0, n_char ) << "\n";

		// Go through each character in string
		for( int j=0; j < n_char; j++ ) {

			// Get character
			str_char = string[i].substr( j, 1 );

//		Rcpp::Rcout << "{" << str_char << "}";

			switch(str_type){
				case 1:

//		Rcpp::Rcout << "[" << f_string << "]\n";

					if(str_char == " " || str_char == "f" || str_char == "#" || str_char == "*"){
					
						// Add to vector
						f_vec.push_back( std::stod(f_string) );
				
						// Clear f_string
						f_string = "";

					}else{

						if(str_char == "/"){
				
							if(char1_b){

								// Add to vector
								f_vec.push_back( std::stod(f_string) );
				
								// Clear f_string
								f_string = "";

								// Reset face forward slash detector
								char1_b = FALSE;
							}else{
								char1_b = TRUE;
							}

						}else{
				
							// Add character to string
							f_string = f_string + str_char;
						}
					}

					break;

				case 2:

//		Rcpp::Rcout << "[" << l_string << "]\n";

					if(str_char == " " || str_char == "l" || str_char == "#" || str_char == "*"){
					
						// Add to vector
						l_vec.push_back( std::stod(l_string) );
				
						// Clear l_string
						l_string = "";

					}else{
				
						// Add character to string
						l_string = l_string + str_char;
					}
					
					break;

				case 3:

					if(str_char == " " || str_char == "v" || str_char == "#" || str_char == "*"){
					
	//		Rcpp::Rcout << "[" << v_string << "]\n";

						// Add to vector
						v_vec.push_back( std::stod(v_string) );
				
						// Clear v_string
						v_string = "";

					}else{

						// Add character to string
						v_string = v_string + str_char;
					}
				
					break;

				case 4:

					if(str_char == " " || str_char == "v" || str_char == "#" || str_char == "*"){
					
	//		Rcpp::Rcout << vn_string << "\n";

						// Add to vector
						vn_vec.push_back( std::stod(vn_string) );
				
						// Clear v_string
						vn_string = "";

					}else{

						// Add character to string
						vn_string = vn_string + str_char;
					}

					break;
			}

			if(str_char == "#") continue;

			if(str_char == "*"){
				str_type = 0;
				char0_s = TRUE;
				continue;
			}

			if(char0_s){

				char0_s = FALSE;

				if(str_char == "v"){
					char1_v = TRUE;
					continue;
				}
				if(str_char == "f"){
					char1_f = TRUE;
					continue;
				}
				if(str_char == "l"){
					char1_l = TRUE;
					continue;
				}
			}

			if(char1_v){
				if(str_char == "n"){
					char2_n = TRUE;
					continue;
				}else if(str_char == " "){
					if(char2_n){
						char2_n = FALSE;
						str_type = 4;
					}else{
						str_type = 3;
					}
				}
				char1_v = FALSE;
			}
			if(char1_f){
				if(str_char == " ") str_type = 1;
				char1_f = FALSE;
			}
			if(char1_l){
				if(str_char == " ") str_type = 2;
				char1_l = FALSE;
			}
		}
    }
    
	return Rcpp::List::create(Rcpp::Named("normals") = vn_vec,
		Rcpp::Named("vertices") = v_vec, Rcpp::Named("faces") = f_vec, 
		Rcpp::Named("lines") = l_vec);
}