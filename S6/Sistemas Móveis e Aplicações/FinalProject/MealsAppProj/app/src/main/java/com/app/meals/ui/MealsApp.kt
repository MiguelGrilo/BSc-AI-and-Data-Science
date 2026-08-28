package com.app.meals.ui

import android.content.res.Configuration
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Home
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBar
import androidx.compose.material3.NavigationBarItem
import androidx.compose.material3.NavigationRail
import androidx.compose.material3.NavigationRailItem
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.windowsizeclass.WindowWidthSizeClass
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.stringResource
import androidx.lifecycle.viewmodel.compose.viewModel
import androidx.navigation.NavDestination.Companion.hierarchy
import androidx.navigation.NavGraph.Companion.findStartDestination
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
import com.app.meals.R
import com.app.meals.ui.screens.DetailsScreen
import com.app.meals.ui.screens.HomeScreen
import com.app.meals.ui.screens.SearchScreen
import com.app.meals.ui.screens.SettingsScreen
import com.app.meals.ui.viewModels.DetailsViewModel
import com.app.meals.ui.viewModels.HomeViewModel
import com.app.meals.ui.viewModels.SearchViewModel
import com.app.meals.ui.viewModels.SettingsViewModel

@Composable
fun MealsApp(
    windowSizeClass: WindowWidthSizeClass,
    modifier: Modifier = Modifier
) {
    val navController = rememberNavController()
    val homeViewModel: HomeViewModel = viewModel(factory = HomeViewModel.Factory)

    val configuration = LocalConfiguration.current
    val isPortrait = configuration.orientation == Configuration.ORIENTATION_PORTRAIT

    val showBottomBar = windowSizeClass == WindowWidthSizeClass.Compact || isPortrait

    Surface(
        modifier = modifier.fillMaxSize(),
        color = MaterialTheme.colorScheme.background
    ) {
        Scaffold(
            bottomBar = {
                if (showBottomBar) {
                    MealsBottomNavigationBar(
                        navController = navController,
                        navigateTo = { route ->
                            navController.navigate(route) {
                                popUpTo(navController.graph.findStartDestination().id) { saveState = true }
                                launchSingleTop = true
                                restoreState = true
                            }
                        }
                    )
                }
            }
        ) { innerPadding ->
            Row(modifier = Modifier.fillMaxSize().padding(innerPadding)) {
                if (!showBottomBar) {
                    MealsNavigationRail(
                        navController = navController,
                        navigateTo = { route ->
                            navController.navigate(route) {
                                popUpTo(navController.graph.findStartDestination().id) { saveState = true }
                                launchSingleTop = true
                                restoreState = true
                            }
                        }
                    )
                }

                NavHost(
                    navController = navController,
                    startDestination = "home",
                    modifier = Modifier.weight(1f)
                ) {
                    composable(route = "home") {
                        HomeScreen(
                            homeUiState = homeViewModel.mealsUiState,
                            onRefresh = { homeViewModel.getMultipleRandomMeals() },
                            retryAction = { homeViewModel.getMultipleRandomMeals() },
                            onMealClick = { mealId -> navController.navigate("details/$mealId") }
                        )
                    }

                    composable(route = "search") {
                        val searchViewModel: SearchViewModel = viewModel(factory = SearchViewModel.Factory)
                        SearchScreen(
                            searchUiState = searchViewModel.uiState,
                            searchQuery = searchViewModel.searchQuery,
                            onQueryChange = searchViewModel::onQueryChanged,
                            onSearchClick = searchViewModel::searchMeals,
                            categories = searchViewModel.categories,
                            selectedCategory = searchViewModel.selectedCategory,
                            onCategoryClick = searchViewModel::onCategorySelected,
                            onMealClick = { mealId -> navController.navigate("details/$mealId") }
                        )
                    }

                    composable(route = "settings") {
                        val settingsViewModel: SettingsViewModel = viewModel(factory = SettingsViewModel.Factory)
                        val isDarkMode by settingsViewModel.isDarkMode.collectAsState()

                        SettingsScreen(
                            isDarkMode = isDarkMode,
                            onThemeToggle = settingsViewModel::toggleTheme,
                            isPortrait = isPortrait
                        )
                    }

                    composable(route = "details/{mealId}") {
                        val detailViewModel: DetailsViewModel = viewModel(factory = DetailsViewModel.Factory)
                        DetailsScreen(
                            detailsUiState = detailViewModel.detailsUiState,
                            retryAction = { detailViewModel.getMealDetails() },
                            onBackClick = { navController.popBackStack() }
                        )
                    }
                }
            }
        }
    }
}

@Composable
fun MealsBottomNavigationBar(navController: androidx.navigation.NavController, navigateTo: (String) -> Unit) {
    val navBackStackEntry by navController.currentBackStackEntryAsState()
    val currentDestination = navBackStackEntry?.destination

    NavigationBar {
        NavigationBarItem(
            icon = { Icon(Icons.Default.Home, contentDescription = null) },
            label = { Text(stringResource(R.string.home)) },
            selected = currentDestination?.hierarchy?.any { it.route == "home" } == true,
            onClick = { navigateTo("Home") }
        )
        NavigationBarItem(
            icon = { Icon(Icons.Default.Search, contentDescription = null) },
            label = { Text(stringResource(R.string.search)) },
            selected = currentDestination?.hierarchy?.any { it.route == "search" } == true,
            onClick = { navigateTo("Search") }
        )
        NavigationBarItem(
            icon = { Icon(Icons.Default.Settings, contentDescription = null) },
            label = { Text(stringResource(R.string.settings)) },
            selected = currentDestination?.hierarchy?.any { it.route == "settings" } == true,
            onClick = { navigateTo("Settings") }
        )
    }
}

@Composable
fun MealsNavigationRail(navController: androidx.navigation.NavController, navigateTo: (String) -> Unit) {
    val navBackStackEntry by navController.currentBackStackEntryAsState()
    val currentDestination = navBackStackEntry?.destination

    NavigationRail(
        modifier = Modifier.fillMaxHeight(),
    ) {
        Column(
            modifier = Modifier
                .fillMaxHeight()
                .padding(vertical = dimensionResource(R.dimen.dimen_24)),
            verticalArrangement = Arrangement.SpaceBetween
        ) {
            NavigationRailItem(
                icon = { Icon(Icons.Default.Home, contentDescription = null) },
                label = { Text(stringResource(R.string.home)) },
                selected = currentDestination?.hierarchy?.any { it.route == "home" } == true,
                onClick = { navigateTo("Home") }
            )
            NavigationRailItem(
                icon = { Icon(Icons.Default.Search, contentDescription = null) },
                label = { Text(stringResource(R.string.search)) },
                selected = currentDestination?.hierarchy?.any { it.route == "search" } == true,
                onClick = { navigateTo("Search") }
            )
            NavigationRailItem(
                icon = { Icon(Icons.Default.Settings, contentDescription = null) },
                label = { Text(stringResource(R.string.settings)) },
                selected = currentDestination?.hierarchy?.any { it.route == "settings" } == true,
                onClick = { navigateTo("Settings") }
            )
        }
    }
}