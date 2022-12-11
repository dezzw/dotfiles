<?php

return [
    'code' => 'en_EN',
    'name' => 'English',
    'general' => [
        'action_copy' => 'Action this item to copy this number to the clipboard',
        'update_available' => 'A new update of this workflow is available',
        'update_available_subtitle' => 'Press return to install the update',
        'update_downloading' => 'Downloading update...',
        'update_downloaded' => 'Download complete',
        'update_installing' => 'installing...',
        'no_updates' => 'You have the latest version',
    ],
    'config' => [
        'value_not_set' => 'Value not set',
        'lang_title' => 'Set base language',
        'lang_subtitle' => 'Configure the base language',
        'currency_title' => 'Add base currency',
        'currency_subtitle' => 'Set a base currency',
        'delete_currency_title' => 'Delete base currency',
        'delete_currency_subtitle' => 'Delete configured base currency',
        'enter_delete_base_currency' => 'Press enter to delete this currency',
        'empty_currency_formats' => 'There are no stored base currency',
        'currency_locale_title' => 'Set currency format',
        'currency_locale_subtitle' => 'Select how you would like to format money',
        'currency_locale_enter' => 'Press return to select this format',
        'crypto_title' => 'Set Coinmarketcap API',
        'crypto_subtitle' => 'Configure your coinmarketcap API Key',
        'fixer_title' => 'Set Fixer API',
        'fixer_subtitle' => 'Configure your fixer API Key',

        'crypto' => 'Cryptocurrency',
        'crypto_add_title' => 'Add custom cryptocurrency',
        'crypto_add_subtitle' => 'Configure custom cryptocurrencies',
        'crypto_remove_title' => 'Remove custom cryptocurrency',
        'crypto_remove_subtitle' => 'Remove custom cryptocurrencies',
        'empty_cryptocurrency' => 'There are no stored custom cryptocurrencies',

        'measurement_title' => 'Set System of Measurement',
        'measurement_subtitle' => 'Define your system of measurement default to metric system',
        'vat_title' => 'Set VAT percentage',
        'vat_input' => 'VAT percentage',
        'vat_subtitle' => 'Configure your default VAT percentage',
        'base_timezone_title' => 'Set base time zone',
        'base_timezone_subtitle' => 'Base time zone to calculate dates in your time',
        'add_date_title' => 'Add date format',
        'add_date_subtitle' => 'Add date format',
        'delete_date_title' => 'Delete date format',
        'delete_date_subtitle' => 'Delete configured date format',
        'empty_date_formats' => 'There are no stored date zones',
        'enter_delete_date' => 'Press enter to delete date format',
        'base_pixels_title' => 'Set base pixels',
        'base_pixels_subtitle' => 'Configure the base pixels for font calculations',
        'enter_delete' => 'Press return to delete',
        'enter_save' => 'Press enter to save configuration',
        'settings_backup_success' => 'Backup created in your Desktop',
        'settings_removed' => 'Settings Removed',
        'settings_set_external' => 'Configured external settings',
        'list_available_title' => 'List available units and currencies',
        'list_available_subtitle' => 'Display a list of available units and currencies',
        'list_currencies_title' => 'List available currencies',
        'list_cryptocurrencies_title' => 'List available crypto currencies',
        'list_units_title' => 'List available units',
        'list_subtitle' => 'Press return to display the list',
        'cache_clear_title' => 'Clear Workflow cache',
        'cache_clear_subtitle' => 'Remove workflow cache in case of errors',
        'cache_deleted' => 'Cache deleted correctly',
        'updates_title' => 'Update Calculate Anything',
        'updates_subtitle' => 'Check for available updates',
        'option_saved' => 'Configuration saved',
        'goback_title' => 'Return to configuration',
        'goback_subtitle' => 'Go back to main menu',
    ],
    'time' => [
        'day' => 'day',
        'days' => 'days',
        'day' => 'day',
        'days' => 'days',
        'year' => 'year',
        'years' => 'years',
        'week' => 'week',
        'weeks' => 'weeks',
        'month' => 'month',
        'months' => 'months',
        'hour' => 'hour',
        'hours' => 'hours',
        'minute' => 'minute',
        'minutes' => 'minutes',
        'second' => 'second',
        'seconds' => 'seconds',
        'microseconds' => 'microseconds',
        'milliseconds' => 'milliseconds',

        'january' => 'january',
        'february' => 'february',
        'march' => 'march',
        'april' => 'april',
        'may' => 'may',
        'june' => 'june',
        'july' => 'july',
        'august' => 'august',
        'september' => 'september',
        'october' => 'october',
        'november' => 'november',
        'december' => 'december',

        'difference_subtitle' => 'Time difference between %s and %s',
        'until_subtitle' => '%s until %s',
        'notvalid_subtitle' => 'Enter a valid date',
        'format_subtitle' => 'Format %s',
        'cmd' => 'Action this item to copy this number to the clipboard',
        'alt' => 'Action this item to copy the amount with no format',
    ],
    'vat' => [
        'vat' => 'VAT',
        'result' => 'VAT of %s = %s',
        'subtitle' => 'VAT of %s',
        'plus' => '%s plus VAT = %s',
        'plus_subtitle' => '%s plus %s of VAT',
        'minus' => '%s minus VAT = %s',
        'minus_subtitle' => '%s minus %s of VAT',
        'cmd' => 'Action this item to copy the amount with no format',
        'empty' => 'Enter a value to process',
    ],
    'percentage' => [
        'result' => '%s is %s of %s',
        'increase' => 'The percentage increase from %s to %s is %s',
        'decrease' => 'The percentage decrease from %s to %s is %s',
        'percentage_of' => '%s of %s is %s', // 10% of 100 is 10
        'cmd' => 'Action this item to copy this number to the clipboard',
        'alt' => 'Action this item to copy the amount with no format',
    ],
    'units' => [
        'length' => 'Length',
        'm' => 'Meter',
        'km' => 'Kilometer',
        'dm' => 'Decimeter',
        'cm' => 'Centimeter',
        'mm' => 'Milimeter',
        'μm' => 'Micrometer',
        'nm' => 'Nanometer',
        'pm' => 'Picometer',
        'in' => 'Inch',
        'ft' => 'Foot',
        'yd' => 'Yard',
        'mi' => 'Mile',
        'h ' => 'Hand',
        'ly' => 'LightYear',
        'au' => 'Astronomical Unit',
        'pc' => 'Parsec',

        'area' => 'Area',
        'm2'  => 'Square Meter',
        'km2' => 'Square Kilometer',
        'cm2' => 'Square Centimeter',
        'mm2' => 'Square Milimeter',
        'ft2' => 'Square Foot',
        'mi2' => 'Square Mile',
        'ha'  => 'hectare',

        'volume'  => 'Volume',
        'l'   => 'Litre',
        'ml'  => 'Mililitre',
        'm3'  => 'Cubic Meter',
        'pt'  => 'Pint',
        'gal' => 'Galon',

        'weight' => 'Weight',
        'kg' => 'Kilogram',
        'gl' => 'Gram',
        'mg' => 'Miligram',
        'N' =>  'Newton',
        'st' => 'Stone',
        'lb' => 'Pound',
        'oz' => 'Ounce',
        't' =>  'Metric Tonne',
        'ukt' => 'UK Long Ton',
        'ust' => 'US short Ton',

        'speed' => 'Speed',
        'mps' => 'Meters per Second',
        'kph' => 'Kilometers Per Hour',
        'mph' => 'Miles Per Hour',

        'rotation' => 'Rotation',
        'deg' => 'Degrees',
        'rad' => 'Radian',

        'temperature' => 'Temperature',
        'k' => 'Kelvin',
        'c' => 'Centigrade',
        'f' => 'Fahrenheit',

        'pressure' => 'Pressure',
        'pa' => 'Pascal',
        'kpa' => 'kilopascal',
        'mpa' => 'MegaPascal',
        'bar' => 'Bar',
        'mbar' => 'Milibar',
        'psi' => 'Pound-force per square inch',

        'time' => 'Time',
        's' => 'Second',
        'year' => 'Year',
        'month' => 'Month',
        'week' => 'Week',
        'day' => 'Day',
        'hr' => 'Hour',
        'min' => 'Minute',
        'ms' => 'Milisecond',
        'μs' => 'Microsecond',
        'ns' => 'Nanosecond',

        'energy' => 'Energy',
        'j' => 'Joule',
        'kj' => 'Kilojoule',
        'mj' => 'Megajoule',
        'cal' => 'Calorie',
        'Nm' => 'Newton Meter',
        'ftlb' => 'Foot Pound',
        'whr' => 'Watt Hour',
        'kwhr' => 'Kilowatt Hour',
        'mwhr' => 'Megawatt Hour',
        'mev' => 'Mega Electron Volt',

        'power' => 'Power',
        'w' => 'Watt',
        'kw' => 'Kilowatts',
        'ps' => 'Metric Horsepower',
        'hp' => 'Mechanical Horsepower',

        'belongs_to' => '%s is a %s unit',
        'error' => 'Error, you can not convert from %s to %s',
        'cmd' => 'Action this item to copy the amount with no format',
        'alt' => 'Action this item to copy this value with unit to the clipboard',
    ],
    'datastorage' => [
        'error' => 'Error, unable to convert to %s',
        'cmd' => 'Presiona enter para copiar el valor al clipboard sin formato',
    ],
    'currency' => [
        'AED' => "United Arab Emirates dirham",
        'AFN' => "Afghan afghani",
        'ALL' => "Albanian lek",
        'AMD' => "Armenian dram",
        'ANG' => "Netherlands Antillean guilder",
        'AOA' => "Angolan kwanza",
        'ARS' => "Argentine peso",
        'AUD' => "Australian dollar",
        'AWG' => "Aruban florin",
        'AZN' => "Azerbaijani manat",
        'BAM' => "Bosnia and Herzegovina convertible mark",
        'BBD' => "Barbados dollar",
        'BDT' => "Bangladeshi taka",
        'BGN' => "Bulgarian lev",
        'BHD' => "Bahraini dinar",
        'BIF' => "Burundian franc",
        'BMD' => "Bermudian dollar",
        'BND' => "Brunei dollar",
        'BOB' => "Boliviano",
        'BRL' => "Brazilian real",
        'BSD' => "Bahamian dollar",
        'BTN' => "Bhutanese ngultrum",
        'BWP' => "Botswana pula",
        'BYN' => "New Belarusian ruble",
        'BYR' => "Belarusian ruble",
        'BZD' => "Belize dollar",
        'CAD' => "Canadian dollar",
        'CDF' => "Congolese franc",
        'CHF' => "Swiss franc",
        'CLF' => "Unidad de Fomento",
        'CLP' => "Chilean peso",
        'CNY' => "Renminbi|Chinese yuan",
        'COP' => "Colombian peso",
        'CRC' => "Costa Rican colon",
        'CUC' => "Cuban convertible peso",
        'CUP' => "Cuban peso",
        'CVE' => "Cape Verde escudo",
        'CZK' => "Czech koruna",
        'DJF' => "Djiboutian franc",
        'DKK' => "Danish krone",
        'DOP' => "Dominican peso",
        'DZD' => "Algerian dinar",
        'EGP' => "Egyptian pound",
        'ERN' => "Eritrean nakfa",
        'ETB' => "Ethiopian birr",
        'EUR' => "Euro",
        'FJD' => "Fiji dollar",
        'FKP' => "Falkland Islands pound",
        'GBP' => "Pound sterling",
        'GEL' => "Georgian lari",
        'GHS' => "Ghanaian cedi",
        'GIP' => "Gibraltar pound",
        'GMD' => "Gambian dalasi",
        'GNF' => "Guinean franc",
        'GTQ' => "Guatemalan quetzal",
        'GYD' => "Guyanese dollar",
        'HKD' => "Hong Kong dollar",
        'HNL' => "Honduran lempira",
        'HRK' => "Croatian kuna",
        'HTG' => "Haitian gourde",
        'HUF' => "Hungarian forint",
        'IDR' => "Indonesian rupiah",
        'ILS' => "Israeli new shekel",
        'INR' => "Indian rupee",
        'IQD' => "Iraqi dinar",
        'IRR' => "Iranian rial",
        'ISK' => "Icelandic króna",
        'JMD' => "Jamaican dollar",
        'JOD' => "Jordanian dinar",
        'JPY' => "Japanese yen",
        'KES' => "Kenyan shilling",
        'KGS' => "Kyrgyzstani som",
        'KHR' => "Cambodian riel",
        'KMF' => "Comoro franc",
        'KPW' => "North Korean won",
        'KRW' => "South Korean won",
        'KWD' => "Kuwaiti dinar",
        'KYD' => "Cayman Islands dollar",
        'KZT' => "Kazakhstani tenge",
        'LAK' => "Lao kip",
        'LBP' => "Lebanese pound",
        'LKR' => "Sri Lankan rupee",
        'LRD' => "Liberian dollar",
        'LSL' => "Lesotho loti",
        'LYD' => "Libyan dinar",
        'MAD' => "Moroccan dirham",
        'MDL' => "Moldovan leu",
        'MGA' => "Malagasy ariary",
        'MKD' => "Macedonian denar",
        'MMK' => "Myanmar kyat",
        'MNT' => "Mongolian tögrög",
        'MOP' => "Macanese pataca",
        'MRO' => "Mauritanian ouguiya",
        'MUR' => "Mauritian rupee",
        'MVR' => "Maldivian rufiyaa",
        'MWK' => "Malawian kwacha",
        'MXN' => "Mexican peso",
        'MXV' => "Mexican Unidad de Inversion",
        'MYR' => "Malaysian ringgit",
        'MZN' => "Mozambican metical",
        'NAD' => "Namibian dollar",
        'NGN' => "Nigerian naira",
        'NIO' => "Nicaraguan córdoba",
        'NOK' => "Norwegian krone",
        'NPR' => "Nepalese rupee",
        'NZD' => "New Zealand dollar",
        'OMR' => "Omani rial",
        'PAB' => "Panamanian balboa",
        'PEN' => "Peruvian Sol",
        'PGK' => "Papua New Guinean kina",
        'PHP' => "Philippine peso",
        'PKR' => "Pakistani rupee",
        'PLN' => "Polish złoty",
        'PYG' => "Paraguayan guaraní",
        'QAR' => "Qatari riyal",
        'RON' => "Romanian leu",
        'RSD' => "Serbian dinar",
        'RUB' => "Russian ruble",
        'RWF' => "Rwandan franc",
        'SAR' => "Saudi riyal",
        'SBD' => "Solomon Islands dollar",
        'SCR' => "Seychelles rupee",
        'SDG' => "Sudanese pound",
        'SEK' => "Swedish krona",
        'SGD' => "Singapore dollar",
        'SHP' => "Saint Helena pound",
        'SLL' => "Sierra Leonean leone",
        'SOS' => "Somali shilling",
        'SRD' => "Surinamese dollar",
        'SSP' => "South Sudanese pound",
        'STD' => "São Tomé and Príncipe dobra",
        'SVC' => "Salvadoran colón",
        'SYP' => "Syrian pound",
        'SZL' => "Swazi lilangeni",
        'THB' => "Thai baht",
        'TJS' => "Tajikistani somoni",
        'TMT' => "Turkmenistani manat",
        'TND' => "Tunisian dinar",
        'TOP' => "Tongan paʻanga",
        'TRY' => "Turkish lira",
        'TTD' => "Trinidad and Tobago dollar",
        'TWD' => "New Taiwan dollar",
        'TZS' => "Tanzanian shilling",
        'UAH' => "Ukrainian hryvnia",
        'UGX' => "Ugandan shilling",
        'USD' => "United States dollar",
        'UYI' => "Uruguay Peso en Unidades Indexadas",
        'UYU' => "Uruguayan peso",
        'UZS' => "Uzbekistan som",
        'VEF' => "Venezuelan bolívar",
        'VND' => "Vietnamese đồng",
        'VUV' => "Vanuatu vatu",
        'WST' => "Samoan tala",
        'XAF' => "Central African CFA franc",
        'XCD' => "East Caribbean dollar",
        'XOF' => "West African CFA franc",
        'XPF' => "CFP franc",
        'YER' => "Yemeni rial",
        'ZAR' => "South African rand",
        'ZMW' => "Zambian kwacha",
        'ZWL' => "Zimbabwean dollar",

        'cmd' => 'Action this item to copy the amount with no format',
        'alt' => 'Action this item to copy the value of 1 to the clipboard',
        'fetch_error' => 'Unable to get currencies data',
        'nofixerapikey_title' => 'You need to configure the API key for Fixer',
        'updating_rates' => 'Updating currency rates...',
    ],
    'crypto_currency' => [
        'cmd' => 'Action this item to copy the amount with no format',
        'alt' => 'Action this item to copy the value of 1 to the clipboard',
        'fetch_error' => 'Unable to get currencies data',
        'noapikey_title' => 'You need to configure the API key for cryptocurrencies',
        'noapikey_subtitle' => 'Please read the documentation for more information',
        'nosymbol_title' => 'The crypto currency is not available, check the docs.',
        'updating_rates' => 'Updating cryptocurrency rates...',
    ]
];
