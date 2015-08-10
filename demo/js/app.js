var app = angular.module('VaultApp', ['ngMaterial', 'ngMdIcons', 'treasure-overlay-spinner', 'ab-base64']).config(['$mdThemingProvider', function($mdThemingProvider) {
    $mdThemingProvider.theme('default')
        .primaryPalette('blue')
        .accentPalette('red');
}]);

app.controller('AppCtrl', ['VaultStorage', '$scope', '$mdDialog', function(VaultStorage, $scope, $mdDialog){

    $scope.credentialSearch = null;
    $scope.noteSearch = null;
    $scope.loading = false;

    $scope.vaultData = null;

    $scope.openVault = function(masterPassword) {
        $scope.loading = true;
        VaultStorage.read(masterPassword).then(function(vault) {
            $scope.vaultData = vault;
            $scope.loading = false;
        });
    }

    function saveVaultData(vaultData) {
        $scope.loading = true;
        VaultStorage.save(vaultData).then(function(vault) {
            $scope.vaultData = vault;
            $scope.loading = false;
        });
    }

    $scope.updateCredentialSearch = function(credentialSearch) {
        $scope.credentialSearch = credentialSearch;
    }

    $scope.updateNoteSearch = function(noteSearch) {
        $scope.noteSearch = noteSearch;
    }

    $scope.credentialInSearch = function(credential) {
        if (!$scope.credentialSearch) {
            return true;
        }

        var searchPhrase = $scope.credentialSearch.toLowerCase();
        if (credential.id.toLowerCase().indexOf(searchPhrase) != -1
            || credential.username.toLowerCase().indexOf(searchPhrase) != -1
            || credential.password.toLowerCase().indexOf(searchPhrase) != -1) {
            return true;
        }

        return false;
    }

    $scope.noteInSearch = function(note) {
        if (!$scope.noteSearch) {
            return true;
        }

        var searchPhrase = $scope.noteSearch.toLowerCase();
        if (note.title != undefined && note.title.toLowerCase().indexOf(searchPhrase) != -1
            || note.content.toLowerCase().indexOf(searchPhrase) != -1) {
            return true;
        }

        return false;
    }

    $scope.onDeleteCredential = function(ev, index) {

        var credential = $scope.vaultData.credentials[index];

        var confirm = $mdDialog.confirm()
          .parent(angular.element(document.body))
          .title("Delete credentials for '" + credential.id + "'")
          .content('Are you sure?')
          .ok('OK')
          .cancel('Cancel')
          .targetEvent(ev);
        $mdDialog.show(confirm).then(function() {
            $scope.vaultData.credentials.splice(index, 1);
            saveVaultData($scope.vaultData);
        });
    }

    $scope.onDeleteNote = function(ev, index) {
        var note = $scope.vaultData.notes[index];

        var confirm = $mdDialog.confirm()
          .parent(angular.element(document.body))
          .title("Delete note '" + (note.title ? note.title : '') + "'")
          .content('Are you sure?')
          .ok('OK')
          .cancel('Cancel')
          .targetEvent(ev);
        $mdDialog.show(confirm).then(function() {
            $scope.vaultData.notes.splice(index, 1);
            saveVaultData($scope.vaultData);
        });
    }

    function showEditCredentialDialog(credential) {
        return $mdDialog.show({
            controller: EditCredentialDialogController,
            templateUrl: 'tpl/edit-credential.html',
            parent: angular.element(document.body),
            locals: {
                credential: credential
            }
        });
    }

    function showEditNoteDialog(note) {
        return $mdDialog.show({
            controller: EditNoteDialogController,
            templateUrl: 'tpl/edit-note.html',
            parent: angular.element(document.body),
            locals: {
                note: note
            }
        });
    }

    $scope.onEditCredential = function(ev, index) {

        var credential = $scope.vaultData.credentials[index];

        showEditCredentialDialog(credential).then(function(credential) {
            $scope.vaultData.credentials[index] = credential;
            saveVaultData($scope.vaultData);
        });
    };

    $scope.onEditNote = function(ev, index) {

        var note = $scope.vaultData.notes[index];

        showEditNoteDialog(note).then(function(note) {
            $scope.vaultData.notes[index] = note;
            saveVaultData($scope.vaultData);
        });
    };

    $scope.onAdd = function(selectedTabIndex) {
        if (selectedTabIndex == 0) {
            showEditCredentialDialog({}).then(function(credential) {
                $scope.vaultData.credentials.push(credential);
                saveVaultData($scope.vaultData);
            });
        } else if (selectedTabIndex == 1) {
            showEditNoteDialog({}).then(function(note) {
                if (!note.title && !note.content) {
                    return;                
                }

                $scope.vaultData.notes.push(note);
                saveVaultData($scope.vaultData);
            });
        }
    }

}]);

function EditCredentialDialogController($scope, $mdDialog, credential) {
    $scope.credential = angular.copy(credential);
    $scope.isNew = !credential.id;

    $scope.cancel = function() {
        $mdDialog.cancel();
    };
    $scope.save = function(credential) {
        $mdDialog.hide(credential);
    };
}

function EditNoteDialogController($scope, $mdDialog, note) {
    $scope.note = angular.copy(note);
    $scope.isNew = !note.title && !note.content;

    $scope.cancel = function() {
        $mdDialog.cancel();
    };
    $scope.save = function(credential) {
        $mdDialog.hide(credential);
    };
}
