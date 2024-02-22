#include "token.h"



#ifdef COMPILER
/*
type_struct2 *SearchSerializedStruct(Array<type_struct2> * ar, std::string name)
{
	FOR(*ar)
	{
		if (it->name == name)
			return it;
	}
	return nullptr;
}
*/
bool IsUnsigned(enum_type2 tp)
{
	switch(tp)
	{
		case enum_type2::TYPE_U8:
		case enum_type2::TYPE_U16:
		case enum_type2::TYPE_U32:
		case enum_type2::TYPE_U64:
		case enum_type2::TYPE_ENUM:
		case enum_type2::TYPE_CHAR:
		case enum_type2::TYPE_BOOL:
		case enum_type2::TYPE_F32:
		case enum_type2::TYPE_F64:
			return true;
		case enum_type2::TYPE_S8:
		case enum_type2::TYPE_S16:
		case enum_type2::TYPE_S32:
		case enum_type2::TYPE_S64:

			return false;
		default:
			ASSERT(false)
	}
	return false;
}
bool CanAddToDataSect(enum_type2 tp)
{
	switch(tp)
	{
		case enum_type2::TYPE_U8:
		case enum_type2::TYPE_U16:
		case enum_type2::TYPE_U32:
		case enum_type2::TYPE_U64:
		case enum_type2::TYPE_S8:
		case enum_type2::TYPE_S16:
		case enum_type2::TYPE_S32:
		case enum_type2::TYPE_S64:
		case enum_type2::TYPE_ENUM:
		case enum_type2::TYPE_F64:
		case enum_type2::TYPE_F32:
		case enum_type2::TYPE_BOOL:
		case enum_type2::TYPE_STRUCT:
			return true;
		default:
			return false;
	}
	return false;
}
int BuiltinTypeSize(enum_type2 tp)
{
	switch(tp)
	{
		case enum_type2::TYPE_U8_TYPE:
		case enum_type2::TYPE_BOOL_TYPE:
		case enum_type2::TYPE_S8_TYPE:
		case enum_type2::TYPE_U8:
		case enum_type2::TYPE_BOOL:
		case enum_type2::TYPE_S8:
			return 1;
		case enum_type2::TYPE_U16_TYPE:
		case enum_type2::TYPE_S16_TYPE:
		case enum_type2::TYPE_U16:
		case enum_type2::TYPE_S16:
			return 2;
		case enum_type2::TYPE_U32_TYPE:
		case enum_type2::TYPE_F32_TYPE:
		case enum_type2::TYPE_S32_TYPE:
		case enum_type2::TYPE_U32:
		case enum_type2::TYPE_F32:
		case enum_type2::TYPE_S32:
		case enum_type2::TYPE_ENUM:
			return 4;
		case enum_type2::TYPE_U64_TYPE:
		case enum_type2::TYPE_S64_TYPE:
		case enum_type2::TYPE_F64_TYPE:
		case enum_type2::TYPE_U64:
		case enum_type2::TYPE_S64:
		case enum_type2::TYPE_F64:
		case enum_type2::TYPE_FUNC:
			return 8;
	}
	ASSERT(false)
	return -1;
}
int GetTypeSize(type2 *tp)
{
	if (tp->ptr > 0)
		return 8;
	
	switch(tp->type)
	{
		case enum_type2::TYPE_REL:
			return BuiltinTypeSize(tp->rel_lhs);
		case enum_type2::TYPE_ARRAY_DYN:
			return 16;
		case enum_type2::TYPE_STATIC_ARRAY:
			return tp->ar_size * GetTypeSize(tp->tp);
		case enum_type2::TYPE_ARRAY:
			return 16;
		case enum_type2::TYPE_U8_TYPE:
		case enum_type2::TYPE_BOOL_TYPE:
		case enum_type2::TYPE_S8_TYPE:
		case enum_type2::TYPE_U8:
		case enum_type2::TYPE_BOOL:
		case enum_type2::TYPE_S8:
		case enum_type2::TYPE_CHAR_TYPE:
		case enum_type2::TYPE_CHAR:
			return 1;
		case enum_type2::TYPE_U16_TYPE:
		case enum_type2::TYPE_S16_TYPE:
		case enum_type2::TYPE_U16:
		case enum_type2::TYPE_S16:
			return 2;
		case enum_type2::TYPE_U32_TYPE:
		case enum_type2::TYPE_F32_TYPE:
		case enum_type2::TYPE_S32_TYPE:
		case enum_type2::TYPE_U32:
		case enum_type2::TYPE_F32:
		case enum_type2::TYPE_S32:
		case enum_type2::TYPE_ENUM:
		case enum_type2::TYPE_ENUM_IDX_32:
			return 4;
		case enum_type2::TYPE_STR_LIT:
		case enum_type2::TYPE_U64_TYPE:
		case enum_type2::TYPE_S64_TYPE:
		case enum_type2::TYPE_F64_TYPE:
		case enum_type2::TYPE_U64:
		case enum_type2::TYPE_S64:
		case enum_type2::TYPE_F64:
		case enum_type2::TYPE_FUNC:
		case enum_type2::TYPE_FUNC_PTR:
			return 8;
		case enum_type2::TYPE_ENUM_TYPE:
		case enum_type2::TYPE_VOID:
			return 0;
		case enum_type2::TYPE_STRUCT_TYPE:
		case enum_type2::TYPE_STRUCT:
			return tp->strct->size;
		default:
			ASSERT(false)
	}
}
#endif

std::string TypeToString(type2 tp)
{
	std::string ret;
	int ptr = 0;
	for(int i = 0; i < tp.ptr; i++)
	{
		ptr++;
		
	}
	if(ptr > 0)
		ret += std::to_string(ptr);

	switch(tp.type)
	{
	case enum_type2::TYPE_ENUM_IDX_32:
	{
		ret += "enum idx ";
	}break;
	case enum_type2::TYPE_STATIC_ARRAY_TYPE:
	case enum_type2::TYPE_STATIC_ARRAY:
	{
		ret += "static ";
		ret += TypeToString(*tp.tp);
		ret += " array";

	}break;;
	case enum_type2::TYPE_VOID_TYPE:
	case enum_type2::TYPE_VOID:
	{
		ret += "void";
	}break;
	case enum_type2::TYPE_S64_TYPE:
	case enum_type2::TYPE_S64:
	{
		ret += "s64";
	}break;
	case enum_type2::TYPE_INT:
	case enum_type2::TYPE_S32_TYPE:
	case enum_type2::TYPE_S32:
	{
		ret += "s32";
	}break;
	case enum_type2::TYPE_S16_TYPE:
	case enum_type2::TYPE_S16:
	{
		ret += "s16";
	}break;
	case enum_type2::TYPE_S8_TYPE:
	case enum_type2::TYPE_S8:
	{
		ret += "s8";
	}break;
	case enum_type2::TYPE_U64_TYPE:
	case enum_type2::TYPE_U64:
	{
		ret += "u64";
	}break;
	case enum_type2::TYPE_U32_TYPE:
	case enum_type2::TYPE_U32:
	{
		ret += "u32";
	}break;
	case enum_type2::TYPE_U16_TYPE:
	case enum_type2::TYPE_U16:
	{
		ret += "u16";
	}break;
	case enum_type2::TYPE_U8_TYPE:
	case enum_type2::TYPE_U8:
	{
		ret += "u8";
	}break;
	case enum_type2::TYPE_CHAR:
	case enum_type2::TYPE_CHAR_TYPE:
	{
		ret += "char";
	}break;
	case enum_type2::TYPE_STR_LIT:
	{
		ret += "str_lit";
	}break;
	case enum_type2::TYPE_F32_TYPE:
	case enum_type2::TYPE_F32:
	{
		ret += "f32";
	}break;
	case enum_type2::TYPE_ENUM:
	{
		ret += std::string("enum ") + tp.e_decl->name;
	}break;
	case enum_type2::TYPE_BOOL:
	{
		ret += "bool";
	}break;
	case enum_type2::TYPE_F64:
	{
		ret += "f64";
	}break;
	case enum_type2::TYPE_TEMPLATE:
		ret += TypeToString(*tp.tp);
		break;
	case enum_type2::TYPE_FUNC_PTR:
	{
		ret += "fn ptr(";
		FOR_VEC(arg, tp.fdecl->args)
		{
			ret += TypeToString((*arg)->type);
			ret += ", ";
		}
		ret.pop_back();
		ret.pop_back();
		ret += ")";

	}break;
	case enum_type2::TYPE_STRUCT_TYPE:
	case enum_type2::TYPE_STRUCT:
	{
		std::string name;
		ret += tp.strct->name.substr();
	}break;
	default:
		ASSERT(false)
	}
	return ret;
}
/*
type_struct2 *GetStructFromTkns(std::vector<token2> *tkns, int *i)
{

	if (globals.struct_scope.size() == 0)
	{
		globals.structs->Init(64);
		globals.struct_scope.reserve(8);
	}
	// skipping struct keyword
	(*i)++;

	type_struct2 *tstrct = globals.structs->Add();
	tstrct->vars.reserve(16);

	bool struct_has_name = (*tkns)[*i].type == tkn_type2::T_WORD;
	if(struct_has_name)
	{
		auto s = (*tkns)[(*i)++].str;

		globals.struct_scope.emplace_back(s);

		// getting strct name based if it's inside another structs
		std::string sname = "";

		for (auto str : globals.struct_scope)
		{
			sname += str;
		}

		tstrct->name = sname.substr();
		globals.struct_scope.emplace_back("::");
	}
	else
	{
		tstrct->name = "";
	}
	
	// skiping curly braces
	(*i)++;

	tkn_type2 semi_colon = tkn_type2::T_SEMI_COLON;

	while(semi_colon == tkn_type2::T_SEMI_COLON)
	{
		if ((*tkns)[*i].type == tkn_type2::T_WORD)
		{
			decl2 out;

			if(GetDeclFromTkns(tkns, i, &out))
				tstrct->vars.emplace_back(out);
		}
		else
		{
			break;
		}
		semi_colon = (*tkns)[(*i)++].type;

	}
	// skiping the close curly;
	*i += 1;
	if(struct_has_name)
	{
		globals.struct_scope.pop_back();
		globals.struct_scope.pop_back();
	}

	return tstrct;
}
*/

std::string TknsToString(std::vector<token2> *tkns)
{
	std::string final_str;
	final_str.reserve(64);
	for(auto t : *tkns)
	{
		final_str.append(t.ToString());
		if (t.type == tkn_type2::T_WORD)
		{
			final_str += " ";
		}
	}
	return final_str;
}
bool IsLetter(char c)
{
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';
}
bool IsNumber(char c)
{
	return c >= '0' && c <= '9';
}
bool GetWordStr(char *input, int sz, int start, std::string *out)
{
	int i = 0;
	bool ret = false;
	char* data = (char*)input;
	char c = data[start + i];

	bool is_first_character_number = IsNumber(data[start]);

	if (is_first_character_number)
	{
		return false;
	}
	while (IsLetter(c) || IsNumber(c) || c == '_')
	{
		i++;
		ret = true;
		c = data[start + i];
	}
	*out = std::string(&data[start], i);

	return ret;
}
long long GetWordNum(char *input, int input_sz, int start, token2 &out)
{
	bool ret = false;
	int i = 0;
	char* data = (char*)input;
	char c = data[start + i];
	char c2 = data[start + i + 2];
	std::string str;

	if (IsNumber(data[start + i]))
	{
		// hex
		if (data[start + i + 1] == 'x')
		{
			i += 2;
			ASSERT(IsLetter(c) || IsNumber(c) || c == '_')

			int cur_idx = start + i;
			int hex_size = 0;
			char c = data[start + i];

			std::vector<unsigned char> numbers;
			while (IsLetter(c) || IsNumber(c) || c == '_')
			{
				if (c != '_')
					numbers.emplace_back(c);

				c = data[start + ++i];
			}

			char cur_nibble = 0;
			long long final_number = 0;

			for (int i = numbers.size() - 1; i >= 0; i--)
			{
				unsigned char n = numbers[i];
				if (IsLetter(n))
				{
					if(n >= 'a' && n <= 'z')
						n = 10 + (n - 'a');
					else
						n = 10 + (n - 'A');
				}
				else if(IsNumber(n))
					n = 0 + (n - '0');
					
				final_number |= ((long long )n << cur_nibble);

				cur_nibble += 4;
			}

			out.type = tkn_type2::T_INT;
			out.i64 = final_number;

			return i;
		}
		else
		{
			bool found_point = false;
			while (c >= '0' && c <= '9' || (c == '.' && c2 != '.'))
			{
				ret = true;
				if (c == '.' && c2 != '.')
				{
					found_point = true;
				}
				i++;
				c = data[start + i];
				c2 = data[start + i + 1];


			}
			if (!ret)
			{
				return 0;
			}
			str = std::string(&data[start], i);

			if (found_point)
			{
				out.type = tkn_type2::T_FLOAT;
				out.f = std::stof(str);
				return i;
			}
			else
			{
				out.type = tkn_type2::T_INT;
				out.i = std::stoi(str);
				return i;
			}
		}
	}

	
	return 0;
}


void EatSpace(char *input, int *i)
{
	while(input[*i] == ' ')
	{
		(*i)++;
	}
}
void Tokenize2(char *input, unsigned int input_sz, std::vector<token2> *tkns, std::vector<char *> *lines_out = nullptr)
{
	*tkns = std::vector<token2>();
	tkns->reserve(256);

	int i = 0;
	int line = 1;
	
	char *line_str = (char *)input;
	line_str = (char*)(&input[i]);
	int cur_line_start_ch = 0;

	if (lines_out)
	{
		lines_out->reserve(256);
		lines_out->emplace_back(line_str);
	}

	bool new_line_was_found = false;
	while(i < input_sz)
	{
		token2 tkn;
		memset(&tkn, 0, sizeof(tkn));
		EatSpace(input, &i);
		tkn.line = line;

		if(new_line_was_found)
			tkn.flags |= TKN_FLAGS_IS_NEW_LINE;

		char *ch = (char *)&(input)[i];
		tkn.line_str = (char *)&input[i];

		

		bool found_char = false;
		switch(ch[0])
		{
			case '~':
			{
				found_char = true;
				tkn.type = T_TILDE;
			}break;
			case '}':
			{
				found_char = true;
				tkn.type = T_CLOSE_CURLY;
			}break;
			case '|':
			{
				found_char = true;
				if(ch[1] == '|')
				{
					tkn.type = T_OR;
					i++;
				}
				else
					tkn.type = T_PIPE;
			}break;
			case '@':
			{
				found_char = true;
				tkn.type = T_AT;
			}break;
			case ':':
			{
				found_char = true;
				tkn.type = T_COLON;
			}break;
			case '/':
			{
				found_char = true;
				// comment
				if(ch[1] == '/')
				{

					i += 2;
					int j = 2;
					char cur_ch = ch[j];
					while(ch[j] != '\n')
					{
						i++;
						j++;
						//ch[i] = 0;
					}
					line++;

					if (lines_out)
						lines_out->emplace_back(line_str);

					found_char = false;
				}
				else
				{
					tkn.type = T_DIV;
				}
			}break;
			case '%':
			{
				found_char = true;
				tkn.type = T_PERCENT;
			}break;
			case '&':
			{
				found_char = true;
				if(ch[1] == '&')
				{
					tkn.type = T_AND;
					i++;
				}
				else
					tkn.type = T_AMPERSAND;
			}break;
			case '=':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_COND_EQ;
					i++;
				}
				else
					tkn.type = T_EQUAL;
			}break;
			case '-':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_MINUS_EQUAL;
					i++;
				}
				else
					tkn.type = T_MINUS;
			}break;
			case '+':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_PLUS_EQUAL;
					i++;
				}
				else
					tkn.type = T_PLUS;
			}break;
			case '{':
			{
				found_char = true;
				tkn.type = T_OPEN_CURLY;
			}break;
			case '<':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_LESSER_EQ;
					i++;
				}
				else
					tkn.type = T_LESSER_THAN;
			}break;
			case '>':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_GREATER_EQ;
					i++;
				}
				else
					tkn.type = T_GREATER_THAN;
			}break;
			case '.':
			{
				found_char = true;
				found_char = true;
				// range operator
				if(ch[1] == '.' && ch[2] != '.')
				{
					tkn.type = T_TWO_POINTS;
					i++;
				}
				// var args
				else if(ch[1] == '.' && ch[2] == '.')
				{
					tkn.type = T_THREE_POINTS;
					i += 2;
				}
				else
					tkn.type = T_POINT;
			}break;
			case '$':
			{
				found_char = true;
				tkn.type = T_DOLLAR;
			}break;
			case '*':
			{
				found_char = true;
				tkn.type = T_MUL;
			}break;
			case ',':
			{
				found_char = true;
				tkn.type = T_COMMA;
			}break;
			case '[':
			{
				found_char = true;
				tkn.type = T_OPEN_BRACKETS;
			}break;
			case ']':
			{
				found_char = true;
				tkn.type = T_CLOSE_BRACKETS;
			}break;
			case '(':
			{
				found_char = true;
				tkn.type = T_OPEN_PARENTHESES;
			}break;
			case '\"':
			{
				int cur_idx = 1;
				//we only want to stop if the find a single quotantion, and not a \" 
				while (ch[cur_idx - 1] != '\\' && ch[cur_idx] != '\"')
				{
					if (ch[cur_idx] == '\n')
					{
						line_str = (char*)(&input[i + 1]);

						if (lines_out)
							lines_out->emplace_back(line_str);
						line++;
					}

					if (ch[cur_idx] == '\\')
					{
						ch[cur_idx] = 255;
						if(ch[cur_idx + 1] == 'n')
						{
							ch[cur_idx + 1] = '\n';
						}
						else
						{
							ASSERT(false)
						}
						cur_idx++;
					}
					cur_idx++;
				}
				
				found_char = true;
				tkn.type = T_STR_LIT;
				tkn.str  = std::string(&input[i + 1], cur_idx - 1);

				i += cur_idx;
			}break;
			case '!':
			{
				found_char = true;
				if(ch[1] == '=')
				{
					tkn.type = T_COND_NE;
					i++;
				}
				else
				{
					tkn.type = T_EXCLAMATION;
				}
			}break;
			case '\'':
			{
				found_char = true;
				tkn.type = T_APOSTROPHE;
				if(input[i] == '\\')
				{
					ASSERT(false)
				}
				else
				{
					tkn.i = input[i + 1];
					i++;
				}

			}break;
			case ')':
			{
				found_char = true;
				tkn.type = T_CLOSE_PARENTHESES;
			}break;
			
			case ';':
			{
				found_char = true;
				tkn.type = T_SEMI_COLON;
			}break;
			case '\t':
			{
			}break;

			case '\r':
			{
				cur_line_start_ch = i + 1;
			}break;
			case '\n':
			{
				input[i] = 0;
				line_str = (char*)(&input[i + 1]);

				if (lines_out)
					lines_out->emplace_back(line_str);

				cur_line_start_ch = i + 1;
				//found_char = true;
				tkn.type = T_NEW_LINE;
				line++;
			}break;
		}
		tkn.line_offset = i - cur_line_start_ch;
		if (found_char)
		{
			tkns->emplace_back(tkn);
			i++;
			continue;
		}
		
		if (GetWordStr(input, input_sz, i, &tkn.str))
		{
			i += tkn.str.length();
			tkns->emplace_back(tkn);
			continue;
		}
		int num_len = GetWordNum(input, input_sz, i, tkn);
		if (num_len > 0)
		{
			i += num_len;
			tkns->emplace_back(tkn);
			continue;
		}
		i++;

	}
	token2 eof;
	eof.type = tkn_type2::T_EOF;
	tkns->emplace_back(eof);
}

bool GetTypeFromTkns(token2 *tkn, type2 &tp)
{
	std::string one_str = tkn->ToString();

	if(one_str == "s64")
	{
		tp.type = enum_type2::TYPE_S64_TYPE;
	}
	else if(one_str == "s32")
	{
		tp.type = enum_type2::TYPE_S32_TYPE;
	}
	else if(one_str == "s16")
	{
		tp.type = enum_type2::TYPE_S16_TYPE;
	}
	else if(one_str == "s8")
	{
		tp.type = enum_type2::TYPE_S8_TYPE;
	}
	else if(one_str == "u64")
	{
		tp.type = enum_type2::TYPE_U64_TYPE;
	}
	else if(one_str == "u32")
	{
		tp.type = enum_type2::TYPE_U32_TYPE;
	}
	else if(one_str == "u16")
	{
		tp.type = enum_type2::TYPE_U16_TYPE;
	}
	else if(one_str == "u8")
	{
		tp.type = enum_type2::TYPE_U8_TYPE;
	}
	else if(one_str == "f64")
	{
		tp.type = enum_type2::TYPE_F64_TYPE;
	}
	else if(one_str == "f32")
	{
		tp.type = enum_type2::TYPE_F32_TYPE;
	}
	else if(one_str == "str_lit")
	{
		tp.type = enum_type2::TYPE_STR_LIT;
	}
	else if(one_str == "char")
	{
		tp.type = enum_type2::TYPE_CHAR_TYPE;
	}
	else if(one_str == "void")
	{
		tp.type = enum_type2::TYPE_VOID_TYPE;
	}
	else if(one_str == "bool")
	{
		tp.type = enum_type2::TYPE_BOOL_TYPE;
	}
	else if(one_str == "_expr")
	{
		tp.type = enum_type2::TYPE_MACRO_EXPR;
	}
	else
	{
		return false;
	}
	return true;
}
/*
bool GetDeclFromTkns(std::vector<token2> *tkns, int *i, decl2 *d)
{
	bool was_struct = IsTknWordStr(&(*tkns)[(*i)], "struct");
	GetTypeFromTkns(tkns, i, d->type);

	tkn_type2 cur = (*tkns)[*i].type;
	// if a struct was as declared, the var name if optional
	if (cur == tkn_type2::T_WORD)
	{
		d->name = (*tkns)[(*i)++].str.substr();
		return true;
	}
	if (was_struct && cur != tkn_type2::T_SEMI_COLON)
	{
		return true;
	}

	return false;
}
*/
