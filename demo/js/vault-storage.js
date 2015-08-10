angular.module('VaultApp').factory('VaultStorage', ['$q', '$http', 'base64', function($q, $http, base64) {

    var demoVaultStorage = {
        read: function(password) {
            if (!window.sessionStorage.vault) {
                window.sessionStorage.vault = angular.toJson({
                    credentials: [{
                        id: 'facebook',
                        username: 'leonti',
                        password: 'r$?tS^>7QD4uq@:s'            
                    }, {
                        id: 'gmail',
                        username: 'leonti',
                        password: '+JK"5V7=D+zGb(A{'
                    }],
                    notes: [{
                        title: 'Account numbers',
                        content: 'Some numbers'
                    }, {
                        title: 'Credit cards',
                        content: 'Some cards'
                    }]        
                });
            }

            return $q.when(angular.fromJson(window.sessionStorage.vault));
        },
        save: function(vault) {
            window.sessionStorage.vault = angular.toJson(vault);
            return demoVaultStorage.read();
        }
    };

    var serverVaultStorage = {
        read: function(password) {

            var authdata = base64.encode('vault:' + password);
            $http.defaults.headers.common['Authorization'] = 'Basic ' + authdata;
            return $http.get('/db').then(function(response) {
                return response.data;
            });
        },

        save: function(vault) {
            return $http.put('/db', vault).then(function() {
                return vault;            
            });
        }
    }; 

    return demoVaultStorage;
//    return serverVaultStorage;
}]);
